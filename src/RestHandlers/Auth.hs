{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module RestHandlers.Auth (verifiedToken, createToken) where

import Data.List (find)
import Data.Maybe (maybe)
import Data.Time.Clock (addUTCTime, NominalDiffTime, getCurrentTime, diffUTCTime, UTCTime (..))
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Web.JWT (
    Algorithm (HS256)
    , claims
    , decodeAndVerifySignature
    , def
    , JWTClaimsSet (JWTClaimsSet)
    , JWT
    , VerifiedJWT
    , Signature
    , NumericDate
    , encodeSigned
    , numericDate
    , secondsSinceEpoch
    , secret
    , stringOrURI
    , iss
    , jti
    , nbf
    , JSON
    )
import Network.Wai (Middleware, mapResponseHeaders, mapResponseStatus)
import Network.Wai.Internal (Request (..), Response (..))
import Network.HTTP.Types (status401)
import RestHandlers.Types (TokenRequest (..))
import Web.Scotty (ActionM, jsonData)
import Control.Monad.IO.Class (liftIO)

expirationTime :: IO NominalDiffTime
expirationTime = utcTimeToPOSIXSeconds . addUTCTime (60 * 60 :: NominalDiffTime) <$> getCurrentTime

createToken :: T.Text -> T.Text -> IO T.Text
createToken secretKey userId = do
  expiration <- expirationTime
  pure $ encodeSigned HS256 (secret secretKey) def {
    iss = stringOrURI "hs-auth"
    , jti = stringOrURI userId
    , nbf = numericDate expiration
  }

checkExpired :: JWTClaimsSet -> IO (Maybe JWTClaimsSet)
checkExpired claimsSet@JWTClaimsSet{ nbf } =
  case nbf of
    (Just expiredTime) -> do
      diffTime <- diffUTCTime ((posixSecondsToUTCTime . secondsSinceEpoch) expiredTime) <$> getCurrentTime
      pure $ if diffTime > 0 then Just claimsSet else Nothing
    Nothing -> pure Nothing

verifiedToken :: T.Text -> T.Text -> Maybe (JWT VerifiedJWT)
verifiedToken key = decodeAndVerifySignature $ secret key
