{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Functions where

import Database.PostgreSQL.Simple
import Data.Aeson
import Data.Text
import Parser

import qualified Data.ByteString.Lazy as B
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)


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

executeFunction :: Connection -> APIFunction -> IO (Maybe Data)
executeFunction _ _ = undefined
