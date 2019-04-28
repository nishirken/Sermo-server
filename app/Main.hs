module Main where

import Server (startServer)
import Config (makeAppConfig)
import Db (makeConnection, prepareDb)

main :: IO ()
main = do
  config <- makeAppConfig
  connection <- makeConnection config
  prepareDb connection
  startServer config connection
