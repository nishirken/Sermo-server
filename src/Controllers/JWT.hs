{-# LANGUAGE OverloadedStrings #-}

module Controllers.JWT (createToken) where

import Data.Time.Clock (addUTCTime, NominalDiffTime, getCurrentTime, UTCTime (..))
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Text.Lazy (Text, toStrict, fromStrict)
import Web.JWT (
    Algorithm (HS256)
    , def
    , JWTClaimsSet (JWTClaimsSet)
    , NumericDate
    , encodeSigned
    , numericDate
    , secret
    , stringOrURI
    , iss
    , jti
    , nbf
    , JSON
    )

secretKey = "my-secret-key"
expirationTime :: IO NominalDiffTime
expirationTime = utcTimeToPOSIXSeconds . (addUTCTime (60 * 60 :: NominalDiffTime)) <$> getCurrentTime

createToken :: Text -> IO Text
createToken email = do
    expiration <- expirationTime
    pure $ fromStrict $ encodeSigned HS256 (secret secretKey) def {
       iss = stringOrURI $ "hs-auth"
       , jti = stringOrURI $ toStrict email
       , nbf = numericDate expiration
   }
