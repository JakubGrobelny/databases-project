{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Parser where

import Data.Aeson
import Data.Foldable

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
    { actionsUser      :: UserData
    , actionType       :: Maybe String
    , actionsProject   :: Maybe Integer
    , actionsAuthority :: Maybe Integer
    } deriving Show

instance FromJSON Actions where
    parseJSON = withObject "Actions" $ \o -> do
        actionsUser      <- parseJSON $ Object o
        actionType       <- o .:? "type"
        actionsProject   <- o .:? "project"
        actionsAuthority <- o .:? "authority"
        return Actions'{..}

data Votes = Votes' 
    { votesUser    :: UserData
    , votesAction  :: Maybe Integer
    , votesProject :: Maybe Integer
    } deriving Show

instance FromJSON Votes where
    parseJSON = withObject "Votes" $ \o -> do
        votesUser    <- parseJSON $ Object o
        votesAction  <- o .:? "action"
        votesProject <- o .:? "project"
        return Votes'{..}

data TrollsTimestamp = TrollsTimestamp { timestamp :: Integer } deriving Show

instance FromJSON TrollsTimestamp where
    parseJSON = withObject "Trolls" $ \o -> do
        timestamp <- o .: "timestamp"
        return TrollsTimestamp{..}

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
    parseJSON = withObject "Function" $ \o -> asum [
        Trolls   <$> o .: "trolls",
        Open     <$> o .: "open",
        Leader   <$> o .: "leader",
        Protest  <$> o .: "protest",
        Support  <$> o .: "support",
        Upvote   <$> o .: "upvote",
        Downvote <$> o .: "downvote",
        Actions  <$> o .: "actions",
        Projects <$> o .: "projects",
        Votes    <$> o .: "votes" ]
