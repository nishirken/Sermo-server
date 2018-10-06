{-# LANGUAGE OverloadedStrings #-}

module ValidationSpec where

import Data.Text.Lazy (pack, snoc, Text)
import Validation (
    validatePasswordMatch
    , validatePasswordLength
    , validateEmailLength
    , minPasswordLength
    , maxPasswordLength
    , maxEmailLength
    , passwordMatchErrorMessage
    , passwordLengthErrorMessage
    , emailErrorMessage
    )
import Test.QuickCheck (
    Arbitrary
    , arbitrary
    , Gen
    , property
    , sized
    , choose
    , forAll
    , elements
    , vectorOf
    )
import Test.Hspec (describe, it, context, SpecWith, shouldBe)

instance Arbitrary Text where
    arbitrary = pack <$> (arbitrary :: Gen String)

type Range = [Int]

sizedText :: Range -> Gen Text
sizedText range = do
    n <- elements range
    pack <$> vectorOf n (arbitrary :: Gen Char)

sizedPasswordRight = sizedText $ map fromIntegral [minPasswordLength..maxPasswordLength]
sizedPasswordLeft = sizedText $ map fromIntegral $ [0..(minPasswordLength - 1)] ++ [(maxPasswordLength + 1)..100]

sizedEmailRight = sizedText $ map fromIntegral [0..maxEmailLength]
sizedEmailLeft = sizedText $ map fromIntegral [(maxEmailLength + 1)..100]

validationSpec :: SpecWith ()
validationSpec =
    describe "Validation" $ do
        context "Password" $ do
            it "Matched the same" $ property $ \x -> validatePasswordMatch (x :: Text) x `shouldBe` Right x
            it "Not matched" $ property $
                \x y -> validatePasswordMatch (x :: Text) (snoc x (y :: Char)) `shouldBe` Left passwordMatchErrorMessage
            it "Validate right length" $ forAll sizedPasswordRight (\x -> validatePasswordLength x `shouldBe` Right x)
            it "Validate left length" $ forAll sizedPasswordLeft
                (\x -> validatePasswordLength x `shouldBe` Left passwordLengthErrorMessage)

        context "Email" $ do
            it "Validate right length" $ forAll sizedEmailRight (\x -> validateEmailLength x `shouldBe` Right x)
            it "Validate left length" $
                forAll sizedEmailLeft (\x -> validateEmailLength x `shouldBe` Left emailErrorMessage)
