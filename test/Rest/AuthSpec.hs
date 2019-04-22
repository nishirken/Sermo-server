{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Rest.AuthSpec (isAuthorizedHandler, createToken) where

import Test.Hspec (context, describe, it, Spec, shouldReturn)
import Rest.Utils (withMockedToken, logInPreparation, preparation)
import Test.Hspec.Wai (with, post, shouldRespondWith)
import Test.Hspec.Wai.JSON (json)
import Data.Text.Encoding (encodeUtf8)
import Db (getUserCredsByEmail)
import RestHandlers.Auth (createToken, isAuthorizedHandler, isTokenValid)

authSpec :: Spec
authSpec = with (fst <$> preparation) $ describe "Authorization" $
  it "Not authorize with wrong token" $
    post (encodeUtf8 "/auth") [json|{ token:"ababa" }|] `shouldRespondWith`
    [json|{ data: false, error: null }|]
