module ArbitraryInstances where

import Test.QuickCheck (Arbitrary (..), Gen)

import Models.Index (JSONError (..), JSONResponse (..), LoginRequest (..), SuccessResponse (..), GraphQLRequest (..))
import Models.TokenObject (TokenObject (..))
import Data.Text as Text

instance Arbitrary JSONError where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary :: Gen (Maybe String)
    pure $ JSONError x (Text.pack <$> y)

instance (Arbitrary a) => Arbitrary (JSONResponse a) where
  arbitrary = do
    x <- arbitrary
    JSONResponse x <$> arbitrary

instance Arbitrary LoginRequest where
  arbitrary = do
    x <- arbitrary :: Gen String
    LoginRequest (Text.pack x) . Text.pack <$> (arbitrary :: Gen String)

instance Arbitrary SuccessResponse where
  arbitrary = SuccessResponse <$> arbitrary

instance Arbitrary GraphQLRequest where
  arbitrary = do
    x <- arbitrary :: Gen String
    GraphQLRequest (Text.pack x) . Text.pack <$> (arbitrary :: Gen String)

instance Arbitrary TokenObject where
  arbitrary = (TokenObject . Text.pack) <$> (arbitrary :: Gen String)
