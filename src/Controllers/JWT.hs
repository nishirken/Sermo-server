{-# LANGUAGE OverloadedStrings #-}

module Controllers.JWT (createToken) where

import Data.Time.Clock (NominalDiffTime)
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
    , JSON
    )
import qualified Web.JWT as JWT

secretKey = "my-secret-key"
expiredSeconds = 60 * 60

createToken :: Text -> Text
createToken email = fromStrict $ encodeSigned HS256 (secret secretKey) def {
    iss = stringOrURI $ "hs-auth"
    , jti = stringOrURI $ toStrict email
    , JWT.exp = numericDate (expiredSeconds :: NominalDiffTime)
}
