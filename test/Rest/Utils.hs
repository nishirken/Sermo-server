{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Rest.Utils where

import Db (makeConnection, prepareDb, clearDb, setUser)
import Routes (routes)
import Config (makeTestConfig, Config (..))
import Network.Wai.Test (SResponse (..))
import Network.Wai (Application, responseLBS)
import Network.HTTP.Types (status200)
import qualified Web.Scotty as Scotty
import qualified Data.Text as Text
import Database.PostgreSQL.Simple (Connection)
import qualified Text.Regex as Regex
import qualified Data.ByteString.Lazy.Char8 as BS

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

logInPreparation :: IO Application
logInPreparation = do
  (app, conn) <- preparation
  setUser conn "test@mail.ru" "right"
  pure app

-- TODO: Fix regex
withMockedToken :: SResponse -> SResponse
withMockedToken SResponse{ simpleStatus, simpleHeaders, simpleBody } = SResponse simpleStatus simpleHeaders newBody
  where newBody = BS.pack $ Regex.subRegex (Regex.mkRegex "\"token\":\".*?\"") (BS.unpack simpleBody) "\"token\":\"mock\"},\"error\""
