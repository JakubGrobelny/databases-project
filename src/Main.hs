{-# LANGUAGE OverloadedStrings #-}

module Main where

import Database.PostgreSQL.Simple
import Data.String
import System.Environment (getArgs)
import System.IO (isEOF)
import Data.Aeson
import Control.Exception
import Parser
import Functions

createConnection :: DatabaseInfo -> IO (Maybe Connection)
createConnection DatabaseInfo {db=name, login=login, dbPasswd=passwd} = do
    conn <- tryConnect (createConnection' name login passwd)
    case conn of
        Left _ -> return Nothing
        Right conn -> return $ Just conn
    where
        tryConnect :: IO Connection -> IO (Either SomeException Connection)
        tryConnect c = try c :: IO  (Either SomeException Connection)
        createConnection' :: String -> String -> String -> IO (Connection)
        createConnection' db login passwd = connect defaultConnectInfo
            { connectDatabase = db
            , connectUser     = login
            , connectPassword = passwd 
            }

queryFromFile :: String -> IO Query
queryFromFile filename = fromString <$> readFile filename

isInit :: [String] -> Bool
isInit ["--init"] = True
isInit _ = False

readInput :: IO (Maybe [APIFunction])
readInput = do
    eof <- isEOF
    if eof
        then return $ Just []
        else do
            input <- getLine
            tail <- readInput
            let function = decode $ fromString input
            return $ function >>= \f ->
                tail >>= \t -> Just $ f : t

splitInput :: [APIFunction] 
           -> ([APIFunction] -> Bool)
           -> Maybe (DatabaseInfo, [APIFunction])
splitInput (Open db : fs) validate =
    if validate fs
        then Just (db, fs)
        else Nothing
splitInput _ _ = Nothing

failAll :: [APIFunction] -> [Maybe a]
failAll = map (const Nothing)

initialize :: [APIFunction] -> IO ([Maybe Data])
initialize input =
    case splitInput input validate of
        Nothing -> return $ failAll input 
        Just (db, fs) -> do
            maybeConn <- createConnection db
            case maybeConn of
                Nothing -> return $ failAll input
                Just conn -> do
                    initQuery <- queryFromFile "src/sql/init.sql"
                    _ <- execute_ conn initQuery
                    results <- runFunctions conn fs
                    close conn
                    return $ Just NoData : results
    where
        validate :: [APIFunction] -> Bool
        validate [] = True
        validate (Leader _ : fs) = validate fs
                
runFunctions :: Connection -> [APIFunction] -> IO ([Maybe Data])
runFunctions _ [] = return []
runFunctions conn (f:fs) = do
    result <- executeFunction conn f
    tail <- runFunctions conn fs
    return $ result : tail

runApp :: [APIFunction] -> IO ([Maybe Data])
runApp input =
    case splitInput input validate of
        Nothing -> return $ failAll input
        Just (db, fs) -> do
            maybeConn <- createConnection db
            case maybeConn of
                Nothing -> return $ failAll input
                Just conn -> do
                    results <- runFunctions conn fs
                    close conn
                    return $ Just NoData : results
    where
        validate :: [APIFunction] -> Bool
        validate [] = True
        validate (Leader _ : fs) = False
        validate (Open _ : fs) = False
        validate (_ : fs) = validate fs

main :: IO ()
main = do
    argc <- getArgs
    let shouldInit = isInit argc
    input <- readInput
    case input of
        Nothing -> putStrLn "Input does not match specification!"
        Just input -> 
            if shouldInit
                then do
                    results <- initialize input
                    mapM_ putStrLn $ map (show . encode . maybeToResult) results
                else do
                    results <- runApp input
                    mapM_ putStrLn $ map (show . encode . maybeToResult) results
