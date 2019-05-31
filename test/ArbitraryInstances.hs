module ArbitraryInstances where

import Test.QuickCheck (Arbitrary (..), Gen)

import Models (
  JSONError (..)
  , JSONResponse (..)
  , LoginRequest (..)
  , SuccessResponse (..)
  , GraphQLRequest (..)
  )
import Models.TokenObject (TokenObject (..))
import Models.GraphQLErrorResponse (GraphQLErrorResponse (..), GraphQLError (..))
import Data.Text as Text

arbitraryText :: Gen Text
arbitraryText = Text.pack <$> (arbitrary :: Gen String)

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
  arbitrary = LoginRequest <$> arbitraryText <*> arbitraryText 

instance Arbitrary SuccessResponse where
  arbitrary = SuccessResponse <$> arbitrary

instance Arbitrary GraphQLRequest where
  arbitrary = GraphQLRequest <$> arbitraryText <*> arbitraryText

instance Arbitrary GraphQLError where
  arbitrary = GraphQLError <$> arbitraryText

instance Arbitrary GraphQLErrorResponse where
  arbitrary = GraphQLErrorResponse <$> (arbitrary :: Gen [GraphQLError])

instance Arbitrary TokenObject where
  arbitrary = TokenObject <$> arbitraryText
