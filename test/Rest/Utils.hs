{-# LANGUAGE OverloadedStrings #-}

module Rest.Utils where

import Db (makeConnection, prepareDb, clearDb)
import Routes (routes)
import Config (makeTestConfig, Config (..))
import Network.Wai (Application, responseLBS)
import Network.HTTP.Types (status200)
import qualified Web.Scotty as Scotty
import qualified Data.Text as Text
import Database.PostgreSQL.Simple (Connection)

testToken :: Text.Text
testToken = ""

preparation :: IO (Application, Connection)
preparation = do
  config <- makeTestConfig
  case config of
    (Right conf) -> do
      connection <- makeConnection conf
      prepareDb connection
      clearDb connection
      app <- Scotty.scottyApp $ routes connection conf
      pure $ (app, connection)
    (Left configError) -> error $ show configError
