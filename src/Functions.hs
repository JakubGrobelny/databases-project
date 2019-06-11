{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Functions where

import Database.PostgreSQL.Simple
import Data.Aeson
import Parser
import Data.String
import qualified Data.ByteString.Lazy as B
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)

queryFromFile :: String -> IO Query
queryFromFile filename = fromString <$> readFile filename

data ResultValue
    = ResultNum Integer
    | ResultBool Bool
    deriving Show

data Tuple = Tuple [ResultValue] deriving Show

data FunctionResult
    = ResultError
    | ResultOK [Tuple]
    | ResultEmptyOK

instance Show FunctionResult where
    show = T.unpack . decodeUtf8 . B.toStrict . encode

instance ToJSON ResultValue where
    toJSON (ResultNum i) = toJSON i
    toJSON (ResultBool b) = toJSON b

instance ToJSON Tuple where
    toJSON (Tuple vals) = toJSON vals

instance ToJSON FunctionResult where
    toJSON ResultError = object [
        "status" .= ("ERROR" :: T.Text) ]    
    toJSON ResultEmptyOK = object [
        "status" .= ("OK" :: T.Text) ]    
    toJSON (ResultOK d) = object [
        "status" .= ("OK" :: T.Text),
        "data" .= toJSON d ]

trollsToFunctionResult :: [(Integer, Integer, Integer, Bool)] -> FunctionResult
trollsToFunctionResult = ResultOK . map (\(m,u,d,a) -> 
   Tuple [ResultNum m, ResultNum u, ResultNum d, ResultBool a])

isUnique :: Connection -> Integer -> IO Bool
isUnique conn id = do
    [Only unique] <- query conn "SELECT is_unique(?)" (Only id)
    return unique

memberExists :: Connection -> Integer -> IO Bool
memberExists conn id = do
    [Only exists] <- query conn "SELECT member_exists(?)" (Only id)
    return exists

projectExists :: Connection -> Integer -> IO Bool
projectExists conn id = do
    [Only exists] <- query conn "SELECT project_exists(?)" (Only id)
    return exists

authorityExists :: Connection -> Integer -> IO Bool
authorityExists conn id = do
    [Only exists] <- query conn "SELECT authority_exists(?)" (Only id)
    return exists

actionExists :: Connection -> Integer -> IO Bool
actionExists conn id = do
    [Only exists] <- query conn "SELECT action_exists(?)" (Only id)
    return exists

voteExists :: Connection -> Integer -> Integer -> IO Bool
voteExists conn memberId actionId = do
    [Only exists] <- query conn "SELECt vote_exists(?,?)" (memberId, actionId)
    return exists

isFrozen :: Connection -> UserData -> IO Bool
isFrozen conn usr = do
    let (id, timestamp) = (member usr, time usr)
    [Only frozen] <- query conn "SELECT is_frozen(?, ?)" (id, timestamp)
    return frozen

isCorrectPassword :: Connection -> UserData -> IO Bool 
isCorrectPassword conn usr = do
    let (id, pass) = (member usr, passwd usr)
    [Only correct] <- query conn "SELECT correct_password(?,?)" (id, pass)
    return correct

isCorrectMember :: Connection -> UserData -> IO Bool
isCorrectMember conn usr = do
    exists <- memberExists conn $ member usr
    if not exists
        then isUnique conn $ member usr
        else do
            frozen <- isFrozen conn usr
            isCorrectPassword <- isCorrectPassword conn usr
            return $ not frozen && isCorrectPassword
                                                           
isLeader :: Connection -> Integer -> IO Bool
isLeader conn memberId = do
    [Only leader] <- query conn "SELECT is_leader(?)" (Only memberId)
    return leader

isCorrectLeader :: Connection -> UserData -> IO Bool
isCorrectLeader conn usr = do
    exists <- memberExists conn $ member usr
    if not exists
        then return False
        else do
            frozen <- isFrozen conn usr
            isCorrectPassword <- isCorrectPassword conn usr
            leader <- isLeader conn $ member usr
            return $ not frozen && isCorrectPassword && leader

addAuthority :: Connection -> Integer -> IO ()
addAuthority conn id = do
    _ <- execute conn "INSERT INTO Authority VALUES (?)" (Only id)
    return ()

addProject :: Connection -> ActionData -> IO ()
addProject conn ActionData{authority=Nothing} = return ()
addProject conn ActionData{authority=Just auth, project=proj} = do
    _ <- execute conn "INSERT INTO Project VALUES(?, ?)" (proj, auth)
    return ()

addMember :: Connection -> UserData -> Bool -> IO ()
addMember conn UserData{member=mem,passwd=pass,time=timestamp} isLeader = do
    _ <- execute conn "INSERT INTO Member VALUES \
        \(?, (SELECT crypt(?, gen_salt('md5'))), to_timestamp(?), ?)" 
        (mem, pass, timestamp, isLeader)
    return ()

addAction :: Connection -> NewAction -> Bool -> IO ()
addAction conn newAction isSupport = do
    _ <- execute conn "INSERT INTO Action VALUES (?, ?, ?, ?)" 
        ( actId 
        , isSupport
        , projectId
        , memberId )
    return ()
    where
        actId = action $ newActionData newAction
        projectId = project $ newActionData newAction
        memberId = member $ newActionUser newAction

updateActionVoteCount :: Connection -> Integer -> Bool -> IO ()
updateActionVoteCount conn actionId False = do
    _ <- execute conn "UPDATE Action SET downvotes = downvotes + 1 WHERE id = ?"
        (Only actionId)
    return ()
updateActionVoteCount conn actionId True = do
    _ <- execute conn "UPDATE Action SET upvotes = upvotes + 1 WHERE id = ?"
        (Only actionId)
    return ()
    

addVote :: Connection -> Integer -> Integer -> Bool -> IO ()
addVote conn memberId actionId isUpvote = do
    _ <- execute conn "INSERT INTO Vote VALUES (?, ?, ?)"
        (memberId, actionId, isUpvote)
    updateActionVoteCount conn actionId isUpvote

ensureMemberExists :: Connection -> UserData -> IO ()
ensureMemberExists conn usr = do
    exists <- memberExists conn (member usr)
    if exists
        then return ()
        else do
            addMember conn usr False

ensureAuthorityExists :: Connection -> Maybe Integer -> IO ()
ensureAuthorityExists _ Nothing = return ()
ensureAuthorityExists conn (Just id) = do
    exists <- authorityExists conn id
    if exists
        then return ()
        else addAuthority conn id

ensureProjectExists :: Connection -> ActionData -> IO ()
ensureProjectExists conn act = do
    exist <- projectExists conn $ project act
    if exist
        then return ()
        else do
            ensureAuthorityExists conn $ authority act
            addProject conn act

locallyUniqueIdentifiers :: APIFunction -> Bool
locallyUniqueIdentifiers (Support action) = 
    locallyUniqueIdentifiers $ Protest action
locallyUniqueIdentifiers (Protest actionData) =
    case authority $ newActionData actionData of
        Nothing -> (proj /= mem && proj /= act && act  /= mem)
        Just auth -> (proj /= auth && proj /= act && proj /= mem 
                    && mem /= auth && mem /= act  && act /= auth)
    where
        proj = project $ newActionData actionData
        mem = member $ newActionUser actionData
        act = action $ newActionData actionData
locallyUniqueIdentifiers (Upvote vote) =
    locallyUniqueIdentifiers $ Downvote vote
locallyUniqueIdentifiers (Downvote vote) =
    (member $ newVoteUser vote) /= (voteAction vote)
locallyUniqueIdentifiers _ = True

isCorrectAuthority :: Connection -> Maybe Integer -> IO Bool
isCorrectAuthority _ Nothing = return False
isCorrectAuthority conn (Just auth) = do
    exists <- authorityExists conn auth
    unique <- isUnique conn auth
    return (exists || unique)

isCorrectProject :: Connection -> ActionData -> IO Bool
isCorrectProject conn act = do
    exists <- projectExists conn $ project act
    unique <- isUnique conn $ project act
    correctAuthority <- isCorrectAuthority conn $ authority act
    return (exists || (unique && correctAuthority))

canAddAction :: Connection -> ActionData -> UserData -> IO Bool
canAddAction conn act usr = do
    uniqueAction <- isUnique conn $ action act
    correctMember <- isCorrectMember conn usr
    correctProject <- isCorrectProject conn act
    return $ uniqueAction && correctMember && correctProject

updateMemberTime :: Connection -> UserData -> IO ()
updateMemberTime conn usr = do
    _ <- execute conn "UPDATE Member SET last_activity = to_timestamp(?) \
        \WHERE id = ?" (t, id)
    return ()
    where
        t = time usr
        id = member usr

newAction :: Connection -> NewAction -> Bool -> IO FunctionResult
newAction conn actionData isSupport = do
    let (act, usr) = (newActionData actionData, newActionUser actionData)
    let locallyUnique = locallyUniqueIdentifiers $ Support actionData
    canAdd <- canAddAction conn act usr
    if not $ locallyUnique && canAdd
        then return ResultError
        else do
            ensureProjectExists conn act
            ensureMemberExists conn usr
            addAction conn actionData isSupport
            updateMemberTime conn usr
            return ResultEmptyOK

canAddVote :: Connection -> Integer -> UserData -> IO Bool
canAddVote conn actionId usr = do
    validAction <- actionExists conn actionId
    correctMember <- isCorrectMember conn usr
    unique <-  voteExists conn (member usr) actionId
    return . not $ validAction && correctMember && unique

newVote :: Connection -> NewVote -> Bool -> IO FunctionResult
newVote conn v isUpvote = do
    let (usr, actionId) = (newVoteUser v, voteAction v)
    let locallyUnique = locallyUniqueIdentifiers $ Upvote v
    canAdd <- canAddVote conn actionId usr
    if not $ locallyUnique && canAdd
        then return ResultError
        else do
            ensureMemberExists conn usr
            addVote conn (member usr) actionId isUpvote
            updateMemberTime conn usr
            return ResultEmptyOK

executeFunction :: Connection -> APIFunction -> IO FunctionResult
executeFunction conn (Leader usr) = do
    unique <- isUnique conn $ member usr
    if not unique
        then return ResultError
        else do
            addMember conn usr True
            return $ ResultEmptyOK
executeFunction conn (Support action) = newAction conn action True
executeFunction conn (Protest action) = newAction conn action False
executeFunction conn (Upvote vote) = newVote conn vote True
executeFunction conn (Downvote vote) = newVote conn vote False
executeFunction conn (Trolls t) = do
    tuples <- query conn "SELECT * FROM trolls(to_timestamp(?)::TIMESTAMP)" $ 
        Only $ timestamp t
    return $ trollsToFunctionResult tuples