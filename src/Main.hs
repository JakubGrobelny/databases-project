{-# LANGUAGE OverloadedStrings #-}

module Main where

import Database.PostgreSQL.Simple

connectionInfo :: IO (Connection)
connectionInfo = connect defaultConnectInfo 
    {
      connectDatabase = "testdb"
    , connectUser     = "admin"
    }

main :: IO Int
main = do
    conn <- connectionInfo
    [Only i] <- query_ conn "select 2 + 2"
    putStrLn $ show i
    return i