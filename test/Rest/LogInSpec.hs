{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Rest.LoginSpec (loginSpec) where

import Test.Hspec (context, describe, it, Spec)
import Test.Hspec.Wai (with, post, shouldRespondWith)
import Test.Hspec.Wai.JSON (json)
import Data.Text.Encoding (encodeUtf8)
import Utils (preparation, withMockedToken, loginPreparation)

incorrectError = [json|{
  data:null,
  error: {
    code: 422,
    message: "Incorrect password or email"
  }
}|]

loginSpec :: Spec
loginSpec = with loginPreparation $ describe "Login" $ do
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
