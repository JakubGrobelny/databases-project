{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson (decode, encode)

data APIFunction
    = Open {db :: String, login :: String, passwd :: String}
    | Leader {time :: Integer, passwd :: Integer, member :: Integer}
    | 



