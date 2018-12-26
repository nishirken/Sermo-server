{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Server (startServer) where

import Web.Scotty (scotty)
import Control.Monad.IO.Class (liftIO)
import Routes (routes)
import Database.PostgreSQL.Simple (Connection)
import Config (Config (..), makeConfig)
import Data.Text.Lazy (toStrict, pack)
import Data.Text.Encoding (encodeUtf8)

startServer :: Config -> Connection -> IO ()
startServer conf@Config{ appPort } dbConn = do
  putStrLn $ "Server started at " <> show appPort
  r <- routes dbConn conf
  scotty appPort r
