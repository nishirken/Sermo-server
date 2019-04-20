{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NamedFieldPuns #-}

module Rest.LogInSpec (logInSpec) where

import Test.Hspec (context, describe, it, Spec, beforeWith, runIO)
import RestHandlers.LogIn (verify)
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Network.Wai (Application)
import Network.Wai.Test (SResponse (..))
import Test.Hspec.Wai (with, post, shouldRespondWith, matchStatus)
import Test.Hspec.Wai.JSON (json)
import Rest.Utils (preparation)
import Db (setUser, getUserCredsByEmail)
import RestHandlers.Auth (createToken)
import Database.PostgreSQL.Simple (Connection)
import qualified Text.Regex as Regex
import qualified Data.ByteString.Lazy.Char8 as BS

incorrectError = [json|{
  data:null,
  error: {
    code: 422,
    message: "Incorrect password or email"
  }
}|]

logInPreparation :: IO Application
logInPreparation = do
  (app, conn) <- preparation
  setUser conn "test@mail.ru" "right"
  pure app

-- TODO: Fix regex
withMockedToken :: SResponse -> SResponse
withMockedToken SResponse{ simpleStatus, simpleHeaders, simpleBody } = SResponse simpleStatus simpleHeaders newBody
  where newBody = BS.pack $ Regex.subRegex (Regex.mkRegex "\"token\":\".*?\"") (BS.unpack simpleBody) "\"token\":\"mock\"},\"error\""

logInSpec :: Spec
logInSpec = with logInPreparation $ describe "LogIn" $ do
    it "Response 422 with incorrect email" $
      post (encodeUtf8 "/login") [json|{email: "incorrect@mail.ru", password: "right"}|]
        `shouldRespondWith` incorrectError
    it "Respond 422 with incorrect password" $
      post (encodeUtf8 "/login") [json|{email: "test@mail.ru", password: "incorrect"}|]
        `shouldRespondWith` incorrectError
    it "Respond token if creds are right" $ (do
        resp <- post
          (encodeUtf8 "/login")
          [json|{email: "test@mail.ru", password: "right"}|]
        pure $ withMockedToken resp)`shouldRespondWith` [json|{
          data: {
            token: "mock"
          },
          error: null
        }|]
