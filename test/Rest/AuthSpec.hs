{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Rest.AuthSpec (authSpecIO) where

import Test.Hspec (context, describe, it, Spec, shouldReturn)
import Rest.Utils (withMockedToken, preparation)
import Test.Hspec.Wai (with, post, shouldRespondWith)
import Test.Hspec.Wai.JSON (json)
import Data.Text.Encoding (encodeUtf8)
import qualified Db
import RestHandlers.Auth (createToken, isAuthorizedHandler, isTokenValid)
import Control.Monad.IO.Class (liftIO)

authSpecIO :: Spec
authSpecIO = with (fst <$> preparation) $ describe "Authorization" $
  it "Not authorize with wrong token" $
    post (encodeUtf8 "/auth") [json|{ token:"ababa" }|] `shouldRespondWith` [json|{ data: false, error: null }|]
  it "Authorize with right token" $ do
    token <- liftIO (do
      rows <- Db.getUserCredsByEmail
      pure $ generatedToken "authKey" (fst $ head rows))
    post (encodeUtf8 "/auth") [json|{ token:"" }|] `shouldRespondWith`
    [json|{ data: true, error: null }|]
