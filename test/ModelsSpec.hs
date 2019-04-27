module ModelsSpec (modelsSpec) where

import Test.Hspec (context, describe, it, Spec, shouldReturn)
import Models.Index (JSONError (..), JSONResponse (..), SuccessResponse (..), GraphQLRequest (..), LoginRequest (..))
import Models.TokenObject (TokenObject (..))
import Test.QuickCheck (property)
import ArbitraryInstances
import qualified Data.Yaml as Yaml
import qualified Data.Aeson as Aeson

type TestJSON a = a -> Bool

testJSON :: (Yaml.FromJSON a, Yaml.ToJSON a, Eq a) => TestJSON a
testJSON model = case (Aeson.decode . Aeson.encode) model of
  (Just m) -> m == model
  Nothing -> False

modelsSpec :: Spec
modelsSpec = describe "Common rest models" $ do
  it "JSONError model" $ property (testJSON :: TestJSON JSONError)
  it "JSONResponse model" $ property (testJSON :: TestJSON (JSONResponse GraphQLRequest))
  it "SuccessResponse model" $ property (testJSON :: TestJSON SuccessResponse)
  it "LoginRequest model" $ property (testJSON :: TestJSON LoginRequest)
  it "GraphQLRequest model" $ property (testJSON :: TestJSON GraphQLRequest)
  it "TokenObject model" $ property (testJSON :: TestJSON TokenObject)
