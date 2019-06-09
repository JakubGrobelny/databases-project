{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Parser where

import Data.Aeson

data DatabaseInfo = DatabaseInfo 
    { db       :: String
    , login    :: String
    , dbPasswd :: String
    } deriving Show

instance FromJSON DatabaseInfo where
    parseJSON = withObject "DatabaseInfo" $ \o -> do
        db       <- o .: "database"
        login    <- o .: "login"
        dbPasswd <- o .: "password"
        return DatabaseInfo{..}

data UserData = UserData 
    { time   :: Integer
    , passwd :: String
    , member :: Integer 
    } deriving Show

instance FromJSON UserData where
    parseJSON = withObject "UserData" $ \o -> do
        time   <- o .: "timestamp"
        passwd <- o .: "password"
        member <- o .: "member"
        return UserData{..}

data ActionData = ActionData 
    { action    :: Integer
    , project   :: Integer
    , authority :: Maybe Integer
    } deriving Show

instance FromJSON ActionData where
    parseJSON = withObject "ActionData" $ \o -> do
        action    <- o .:  "action"
        project   <- o .:  "project"
        authority <- o .:? "authority"
        return ActionData{..}

data NewAction = NewAction 
    { newActionUser :: UserData
    , newActionData :: ActionData
    } deriving Show

instance FromJSON NewAction where
    parseJSON = withObject "Action" $ \o -> do
        newActionUser <- parseJSON $ Object o
        newActionData <- parseJSON $ Object o
        return NewAction{..}

data NewVote = NewVote 
    { newVoteUser :: UserData
    , voteAction  :: Integer 
    } deriving Show

instance FromJSON NewVote where
    parseJSON = withObject "Vote" $ \o -> do
        newVoteUser <- parseJSON $ Object o
        voteAction  <- o .: "action"
        return NewVote{..}

data Projects = Projects' 
    { projectsUser      :: UserData
    , projectsAuthority :: Maybe Integer
    } deriving Show

instance FromJSON Projects where
    parseJSON = withObject "Projects" $ \o -> do
        projectsUser      <- parseJSON $ Object o
        projectsAuthority <- o .:? "authority"
        return Projects'{..}

data ActionsFilter
    = ActionsProject Integer
    | ActionsAuthority Integer
    deriving Show

data Actions = Actions' 
    { actionsUser   :: UserData
    , actionType    :: Maybe String
    , actionsFilter :: Maybe ActionsFilter
    } deriving Show

instance FromJSON Actions where
    parseJSON = withObject "Actions" $ \o -> do
        actionsUser <- parseJSON $ Object o
        actionType  <- o .:? "type"
        project     <- o .:? "project"
        case project of
            Nothing -> do
                authority <- o .:? "authority"
                let filter = authority >>= Just . ActionsAuthority
                return $ Actions' actionsUser actionType filter
            Just p -> do
                let filter = Just $ ActionsProject p
                return $ Actions' actionsUser actionType filter

data VotesFilter
    = VotesAction Integer
    | VotesProject Integer
    deriving Show

data Votes = Votes' 
    { votesUser :: UserData
    , votesFilter :: Maybe VotesFilter 
    } deriving Show

instance FromJSON Votes where
    parseJSON = withObject "Votes" $ \o -> do
        votesUser <- parseJSON $ Object o
        action <- o .:? "action"
        case action of
            Nothing -> do
                project <- o .:? "project"
                let filter = project >>= Just . VotesProject
                return $ Votes' votesUser filter
            Just a -> do
                let filter = Just $ VotesAction a
                return $ Votes' votesUser filter

data TrollsTimestamp = TrollsTimestamp { timestamp :: Integer } deriving Show

instance FromJSON TrollsTimestamp where
    parseJSON = withObject "Trolls" $ \o -> do
        timestamp <- o .: "timestamp"
        return TrollsTimestamp{..}

data APIFunctionJSON = APIFunctionJSON
    { trolls   :: Maybe TrollsTimestamp
    , open     :: Maybe DatabaseInfo
    , leader   :: Maybe UserData
    , protest  :: Maybe NewAction
    , support  :: Maybe NewAction
    , upvote   :: Maybe NewVote
    , downvote :: Maybe NewVote
    , actions  :: Maybe Actions
    , projects :: Maybe Projects
    , votes    :: Maybe Votes
    } deriving Show

instance FromJSON APIFunctionJSON where
    parseJSON = withObject "Function" $ \o -> do
        trolls   <- o .:? "trolls"
        open     <- o .:? "open"
        leader   <- o .:? "leader"
        protest  <- o .:? "protest"
        support  <- o .:? "support"
        upvote   <- o .:? "upvote"
        downvote <- o .:? "downvote"
        actions  <- o .:? "actions"
        projects <- o .:? "projects"
        votes    <- o .:? "votes"
        return APIFunctionJSON{..}

data APIFunction
    = Trolls TrollsTimestamp
    | Open DatabaseInfo
    | Leader UserData
    | Protest NewAction
    | Support NewAction
    | Upvote NewVote
    | Downvote NewVote 
    | Actions Actions
    | Projects Projects
    | Votes Votes
    deriving Show

instance FromJSON APIFunction where
    parseJSON = withObject "Function" $ \o -> do
        functions <- parseJSON $ Object o
        case extractAPIFunction functions of
            Nothing -> error "Invalid JSON input!"
            Just f  -> return f

extractAPIFunction :: APIFunctionJSON -> Maybe APIFunction
extractAPIFunction APIFunctionJSON {trolls = Just t} = Just $ Trolls t
extractAPIFunction APIFunctionJSON {open = Just o} = Just $ Open o
extractAPIFunction APIFunctionJSON {leader = Just l} = Just $ Leader l
extractAPIFunction APIFunctionJSON {protest = Just p} = Just $ Protest p
extractAPIFunction APIFunctionJSON {support = Just s} = Just $ Support s
extractAPIFunction APIFunctionJSON {upvote = Just u} = Just $ Upvote u
extractAPIFunction APIFunctionJSON {downvote = Just d} = Just $ Downvote d
extractAPIFunction APIFunctionJSON {actions = Just a} = Just $ Actions a
extractAPIFunction APIFunctionJSON {projects = Just p} = Just $ Projects p
extractAPIFunction APIFunctionJSON {votes = Just v} = Just $ Votes v
extractAPIFunction _ = Nothing
