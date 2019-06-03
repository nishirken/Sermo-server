{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module ModelsSpec (modelsSpec) where

import Test.Hspec (context, describe, it, Spec, shouldBe)
import Models (
  JSONError (..)
  , JSONResponse (..)
  , SuccessResponse (..)
  , GraphQLRequest (..)
  , LoginRequest (..)
  )
import Models.AuthResponse (AuthResponse (..))
import Models.AuthRequest (AuthRequest (..))
import Test.QuickCheck (property)
import ArbitraryInstances
import qualified Data.Yaml as Yaml
import qualified Data.Aeson as Aeson
import Data.Aeson.QQ (aesonQQ)

type TestJSON a = a -> Bool

testJSON :: (Yaml.FromJSON a, Yaml.ToJSON a, Eq a) => TestJSON a
testJSON model = case (Aeson.decode . Aeson.encode) model of
  (Just m) -> m == model
  Nothing -> False

modelsSpec :: Spec
modelsSpec = describe "Common rest models" $ do
  context "JSONError model" $ do
    it "JSON encode decode" $ property (testJSON :: TestJSON JSONError)
    it "Success decode" $
      Aeson.toJSON (JSONError 200 $ Just "message") `shouldBe` [aesonQQ|{ code: 200, message: "message" }|]
  context "JSONResponse model" $ do
    it "JSON encode decode" $ property (testJSON :: TestJSON (JSONResponse GraphQLRequest))
    it "Success decode" $
      Aeson.toJSON (JSONResponse (Just ("data" :: String)) Nothing) `shouldBe`
        [aesonQQ|{ data: "data", error: null }|]
  context "SuccessResponse model" $ do
    it "JSON encode decode" $ property (testJSON :: TestJSON SuccessResponse)
    it "Success decode" $
      Aeson.toJSON (SuccessResponse True) `shouldBe` [aesonQQ|{ success: true }|]
  context "LoginRequest model" $ do
    it "JSON encode decode" $ property (testJSON :: TestJSON LoginRequest)
    it "Success decode" $ Aeson.toJSON (LoginRequest "mail@test.ru" "123456") `shouldBe`
      [aesonQQ|{ email: "mail@test.ru", password: "123456" }|]
  context "GraphQLRequest model" $ do
    it "JSON encode decode" $ property (testJSON :: TestJSON GraphQLRequest)
    it "Success decode" $ Aeson.toJSON (GraphQLRequest "token22" "query { userId }") `shouldBe`
      [aesonQQ|{ token: "token22", body: "query { userId }" }|]
  context "AuthRequest model" $ do
    it "JSON encode decode" $ property (testJSON :: TestJSON AuthRequest)
    it "Success decode" $ 
      Aeson.toJSON (AuthRequest "token22") `shouldBe` [aesonQQ|{ token: "token22" }|]
  context "AuthResponse model" $ do
    it "JSON encode decode" $ property (testJSON :: TestJSON AuthResponse)
    it "Success decode" $ 
      Aeson.toJSON (AuthResponse 22 "token22") `shouldBe` [aesonQQ|{ id: 22, token: "token22" }|]
