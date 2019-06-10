{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Functions where

import Database.PostgreSQL.Simple
import Data.Aeson
import Data.Text
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

data Data 
    = Data [Tuple]
    | NoData
    deriving Show

data FunctionResult
    = ResultError
    | ResultOk [Tuple]
    | ResultEmptyOK

instance Show FunctionResult where
    show = T.unpack . decodeUtf8 . B.toStrict . encode

maybeToResult :: Maybe Data -> FunctionResult
maybeToResult Nothing = ResultError
maybeToResult (Just NoData) = ResultEmptyOK
maybeToResult (Just (Data d)) = ResultOk d

instance ToJSON ResultValue where
    toJSON (ResultNum i) = toJSON i
    toJSON (ResultBool b) = toJSON b

instance ToJSON Tuple where
    toJSON (Tuple vals) = toJSON vals

instance ToJSON FunctionResult where
    toJSON ResultError = object [
        "status" .= ("ERROR" :: Text) ]    
    toJSON ResultEmptyOK = object [
        "status" .= ("OK" :: Text) ]    
    toJSON (ResultOk d) = object [
        "status" .= ("OK" :: Text),
        "data" .= toJSON d ]

isUnique :: Connection -> Integer -> IO Bool
isUnique conn id = do
    [Only unique] <- query conn "SELECT is_unique(?)" (Only id)
    return unique

executeFunction :: Connection -> APIFunction -> IO FunctionResult
executeFunction conn (Leader usr) = do
    unique <- isUnique conn $ member usr
    if not unique
        then return ResultError
        else do
            query <- queryFromFile "src/sql/add_user.sql"
            _ <- execute conn query (member usr, passwd usr, time usr, True)
            return $ ResultEmptyOK

-- executeFunction conn (Leader user) = 


