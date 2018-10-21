{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Handlers.Auth (authMiddleware, createToken) where

import Control.Monad.IO.Class (liftIO)
import Data.List (find)
import Data.Maybe (maybe)
import Data.Time.Clock (addUTCTime, NominalDiffTime, getCurrentTime, diffUTCTime, UTCTime (..))
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import qualified Data.Text.Lazy as TLazy
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

expirationTime :: IO NominalDiffTime
expirationTime = utcTimeToPOSIXSeconds . addUTCTime (60 * 60 :: NominalDiffTime) <$> getCurrentTime

createToken :: TLazy.Text -> TLazy.Text -> IO TLazy.Text
createToken secretKey userId = do
    expiration <- expirationTime
    pure $ TLazy.fromStrict $ encodeSigned HS256 (secret $ TLazy.toStrict secretKey) def {
       iss = stringOrURI "hs-auth"
       , jti = stringOrURI $ TLazy.toStrict userId
       , nbf = numericDate expiration
   }

checkExpired :: JWTClaimsSet -> IO (Maybe JWTClaimsSet)
checkExpired claimsSet@JWTClaimsSet{ nbf } =
    case nbf of
        (Just expiredTime) -> do
            diffTime <- diffUTCTime ((posixSecondsToUTCTime . secondsSinceEpoch) expiredTime) <$> getCurrentTime
            pure $ if diffTime > 0 then Just claimsSet else Nothing
        Nothing -> pure Nothing

verifiedToken :: TLazy.Text -> TLazy.Text -> Maybe (JWT VerifiedJWT)
verifiedToken key header = tokenFromHeader >>= \token -> decodeAndVerifySignature (secret $ TLazy.toStrict key) (TLazy.toStrict token)
    where
        tokenFromHeader = case TLazy.splitOn "=" header of
            [x, y] -> Just y
            _ -> Nothing

isAvailable :: TLazy.Text -> Bool
isAvailable rawPathInfo = rawPathInfo == "/login" || rawPathInfo == "/signin"

authMiddleware :: TLazy.Text -> Middleware
authMiddleware authKey app req@Request{ requestHeaders, rawPathInfo } response =
    if isAvailable ((TLazy.fromStrict . decodeUtf8) rawPathInfo) then app req response else
        let cookieHeader = find (\(headerName, _) -> headerName == "Cookie") requestHeaders
            token = do
                (_, headerValue) <- cookieHeader
                verifiedToken <- verifiedToken authKey $ (TLazy.fromStrict . decodeUtf8) headerValue
                Just verifiedToken
            in case token of
                (Just _) -> app req response
                Nothing -> app req $ response . mapResponseStatus (const status401)
