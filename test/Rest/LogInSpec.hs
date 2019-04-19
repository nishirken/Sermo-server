module Rest.LogInSpec where

import Test.Hspec (context, describe, it, SpecWith, shouldBe)
import Test.QuickCheck (Property, property, generate, Gen, arbitrary)
import Test.QuickCheck.Monadic (monadicIO, run)
import RestHandlers.LogIn (verify)
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import qualified Crypto.Scrypt as Crypto

logInSpec :: SpecWith ()
logInSpec =
  describe "LogIn" $ do
    it "Password verification" $ monadicIO $ do
      password <- generate (arbitrary :: Text.Text)
      passwordHash <- run $ Crypto.encryptPassIO' (Crypto.Pass . encodeUtf8 $ password)
      verify password ((decodeUtf8 . Crypto.getEncryptedPass) passwordHash) `shouldBe` True

