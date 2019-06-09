{-# LANGUAGE OverloadedStrings #-}

module Main where

import Database.PostgreSQL.Simple
import Data.String
import System.Environment (getArgs)
import System.IO (isEOF)
import Data.Aeson
import Parser
import Functions

createConnection :: String -> String -> String -> IO (Connection)
createConnection db login passwd = connect defaultConnectInfo
    { connectDatabase = db
    , connectUser     = login
    , connectPassword = passwd 
    }

queryFromFile :: String -> IO Query
queryFromFile filename = fromString <$> readFile filename

isInit :: [String] -> Bool
isInit [] = False
isInit ["--init"] = True
isInit _ = error "Invalid command line argument"

readInput :: IO [APIFunction]
readInput = do
    eof <- isEOF
    if eof
        then return []
        else do
            input <- getLine
            let function = decode $ fromString input
            case function of
                Nothing -> error "Invalid JSON input!"
                Just f -> do
                    tail <- readInput
                    return $ f : tail

validateInitInput :: [APIFunction] -> (DatabaseInfo, [APIFunction])
validateInitInput (Open db : fs) = 
    if validate fs
        then (db, fs)
        else error "Invalid JSON input!"
    where
        validate :: [APIFunction] -> Bool
        validate [] = True
        validate (Leader _ : fs) = validate fs
        validate _ = False
validateInitInput _ = error "Invalid JSON input!"

initialize :: [APIFunction] -> IO ()
initialize input = do
    let (dbInfo, leaders) = validateInitInput input
    conn <- createConnection (db dbInfo) (login dbInfo) (dbPasswd dbInfo)
    query <- queryFromFile "src/sql/init.sql"
    _ <- execute_ conn query
    createLeaders conn leaders
    close conn 
    where
        createLeaders :: Connection -> [APIFunction] -> IO ()
        createLeaders _ [] = return ()
        createLeaders conn (l:ls) = do
            executeFunction conn l
            createLeaders conn ls
            
main :: IO ()
main = do
    argc <- getArgs
    let shouldInit = isInit argc
    input <- readInput
    if shouldInit
        then initialize input
        else return ()
    
    -- connection <- createConnection "partydb" "init" "abc123" -- just for testing
    -- q <- queryFromFile "src/sql/schema.sql"
    -- _ <- execute_ connection q
    -- [Only i] <- query_ connection "SELECT 2 + 2"
    -- putStrLn $ show i
    -- close connection
    -- return i