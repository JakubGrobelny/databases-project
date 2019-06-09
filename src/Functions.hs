module Functions where

import Database.PostgreSQL.Simple

import Parser

executeFunction :: Connection -> APIFunction -> IO ()
executeFunction _ _ = return ()
