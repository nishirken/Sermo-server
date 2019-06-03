{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Rest.AuthSpec (authSpecIO) where

import Test.Hspec (context, describe, it, Spec, shouldReturn, Expectation)
import TestUtils (withMockedToken, loginPreparation)
import Test.Hspec.Wai (with, post, shouldRespondWith)
import Test.Hspec.Wai.JSON (json, fromValue)
import Data.Text.Encoding (encodeUtf8)
import qualified Db
import Models.DbFullUserCreds (DbFullUserCreds (..))
import Rest.Auth (createToken, isAuthorizedHandler, isTokenValid)
import Models.AuthRequest (AuthRequest (..))
import qualified Data.Yaml as Yaml
import Control.Monad.IO.Class (liftIO)
import Config (makeTestConfig, Config (..))
import qualified Data.Text as Text

makeTestToken :: Config -> IO Text.Text
makeTestToken conf@Config {..} = do
  connection <- Db.makeConnection conf
  [DbFullUserCreds {..}] <- Db.getUserCredsByEmail connection "test@mail.ru"
  createToken authKey $ (Text.pack . show) _id

authSpecIO :: Spec
authSpecIO = with loginPreparation $ describe "Authorization" $ do
  it "Not authorize with wrong token" $
    post (encodeUtf8 "/auth") [json|{ token:"ababa" }|] `shouldRespondWith` [json|{ data: { success: false }, error: null }|]
  it "Authorize with right token" (do
    token <- liftIO (do
      config <- makeTestConfig
      makeTestToken config)
    post (encodeUtf8 "/auth") (fromValue $ Yaml.toJSON $ AuthRequest token) `shouldRespondWith` [json|{ data: { success: true }, error: null }|])
