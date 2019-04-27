{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Rest.AuthSpec (authSpecIO) where

import Test.Hspec (context, describe, it, Spec, shouldReturn, Expectation)
import Rest.Utils (withMockedToken, logInPreparation)
import Test.Hspec.Wai (with, post, shouldRespondWith)
import Test.Hspec.Wai.JSON (json, fromValue)
import Data.Text.Encoding (encodeUtf8)
import qualified Db
import RestHandlers.Auth (createToken, isAuthorizedHandler, isTokenValid)
import RestHandlers.Utils (TokenResponse (..))
import qualified Data.Yaml as Yaml
import Control.Monad.IO.Class (liftIO)
import Config (makeTestConfig, Config (..))
import qualified Data.Text as Text

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

makeTestToken :: Config -> IO Text.Text
makeTestToken conf@Config{ authKey } = do
  connection <- Db.makeConnection conf
  rows <- Db.getUserCredsByEmail connection "test@mail.ru"
  let userId = (Text.pack . show . fst3 . head) rows
  createToken authKey userId

authSpecIO :: Spec
authSpecIO = with logInPreparation $ describe "Authorization" $ do
  it "Not authorize with wrong token" $
    post (encodeUtf8 "/auth") [json|{ token:"ababa" }|] `shouldRespondWith` [json|{ data: { success: false }, error: null }|]
  it "Authorize with right token" (do
    token <- liftIO (do
      config <- makeTestConfig
      makeTestToken config)
    post (encodeUtf8 "/auth") (fromValue $ Yaml.toJSON $ TokenResponse token) `shouldRespondWith` [json|{ data: { success: true }, error: null }|])
