{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Functions where

import Database.PostgreSQL.Simple
import qualified Data.Aeson as Aeson

import Parser

data Value
    = Number Integer
    | Boolean Bool

data Tuple = Tuple [Value]

data Data 
    = Data [Tuple]
    | NoData

executeFunction :: Connection -> APIFunction -> IO (Maybe Data)
executeFunction _ _ = undefined
