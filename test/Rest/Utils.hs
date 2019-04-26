{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Rest.Utils where

import Db (makeConnection, prepareDb, clearDb, setUser)
import Routes (routes)
import Config (makeTestConfig, Config (..))
import Network.Wai.Test (SResponse (..))
import Test.Hspec.Wai.Matcher (ResponseMatcher (..))
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
  connection <- makeConnection config
  prepareDb connection
  clearDb connection
  app <- Scotty.scottyApp $ routes connection config
  pure $ (app, connection)

logInPreparation :: IO Application
logInPreparation = do
  (app, conn) <- preparation
  setUser conn "test@mail.ru" "right"
  pure app

-- TODO: Fix regex
withMockedToken :: SResponse -> SResponse
withMockedToken SResponse{ simpleStatus, simpleHeaders, simpleBody } = SResponse simpleStatus simpleHeaders newBody
  where newBody = BS.pack $ Regex.subRegex (Regex.mkRegex "\"token\":\".*?\"") (BS.unpack simpleBody) "\"token\":\"mock\"},\"error\""

replaceToken :: Text.Text -> BS.ByteString -> BS.ByteString
replaceToken token rawJson =
  BS.pack $ Regex.subRegex (Regex.mkRegex "\\$token") (BS.unpack rawJson) (Text.unpack token)
