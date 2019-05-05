{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Rest.SigninSpec (signinSpecApi, signinSpecDb) where

import Test.Hspec (context, describe, it, Spec, shouldReturn)
import Utils (withMockedToken, loginPreparation, preparation)
import Test.Hspec.Wai (with, post, shouldRespondWith)
import Test.Hspec.Wai.JSON (json)
import Data.Text.Encoding (encodeUtf8)
import qualified Db
import Control.Monad.IO.Class (liftIO)
import Config (makeTestConfig)

signinSpecApi :: Spec
signinSpecApi = with loginPreparation $ describe "Signin Api" $ do
  it "Respond 422 if email already exists" $
    post (encodeUtf8 "/signin") [json|{email: "test@mail.ru", password: "test123"}|] `shouldRespondWith`
      [json|{
        data: null,
        error: {
          code: 422,
          message: "Email already exists"
        }
      }|]
  it "Success with new creds" $ (do
    resp <- post (encodeUtf8 "/signin") [json|{email: "new@mail.ru", password: "right123"}|]
    pure $ withMockedToken resp) `shouldRespondWith`
      [json|{
        data: {
          token: "mock"
        },
        error: null
      }|]

signinSpecDb :: Spec
signinSpecDb = describe "Signin Db" $
  it "Successfully write new user in db" $ (do
    config <- makeTestConfig
    connection <- Db.makeConnection config
    rows <- Db.getUserCredsByEmail connection "new@mail.ru"
    pure $ (length rows /= 0)) `shouldReturn` True
