{-# LANGUAGE OverloadedStrings #-}

module Main where

import Database.PostgreSQL.Simple

main :: IO Int
main = do
    conn <- connectPostgreSQL ""
    [Only i] <- query_ conn "select 2 + 2"
    return i