{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Rest.SignInSpec (signInSpecIO) where

import Test.Hspec (context, describe, it, Spec, shouldReturn)
import Rest.Utils (withMockedToken, logInPreparation, preparation)
import Test.Hspec.Wai (with, post, shouldRespondWith)
import Test.Hspec.Wai.JSON (json)
import Data.Text.Encoding (encodeUtf8)
import Db (getUserCredsByEmail)

signInSpecIO :: Spec
signInSpecIO = with logInPreparation $ describe "SignIn IO" $ do
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
