module Main where

import Server (startServer)
import Config (makeAppConfig)
import Db (makeConnection, prepareDb)

main :: IO ()
main = do
  config <- makeAppConfig
  case config of
    (Right conf) -> do
      connection <- makeConnection conf
      prepareDb connection
      startServer conf connection
    (Left configError) -> print configError
