{-# LANGUAGE OverloadedStrings #-}

module Main where

import Database.PostgreSQL.Simple
import Data.String

import Parser

createConnection :: String -> String -> String -> IO (Connection)
createConnection db login passwd = connect defaultConnectInfo
    { connectDatabase = db
    , connectUser     = login
    , connectPassword = passwd 
    }

queryFromFile :: String -> IO Query
queryFromFile filename = fromString <$> readFile filename

main :: IO Int
main = do
    connection <- createConnection "partydb" "init" "abc123" -- just for testing
    q <- queryFromFile "src/sql/schema.sql"
    _ <- execute_ connection q
    [Only i] <- query_ connection "SELECT 2 + 2"
    putStrLn $ show i
    close connection
    return i