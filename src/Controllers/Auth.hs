{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Controllers.Auth (authMiddleware, createToken) where

import Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import Control.Monad (MonadPlus, mzero)
import Control.Monad.IO.Class (liftIO)
import Data.List (find)
import Data.Maybe (maybe)
import Data.Time.Clock (addUTCTime, NominalDiffTime, getCurrentTime, diffUTCTime, UTCTime (..))
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Data.Text.Lazy (Text, toStrict, fromStrict, splitOn)
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
import Network.HTTP.Types (status302)

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

checkExpired :: JWTClaimsSet -> IO (Maybe JWTClaimsSet)
checkExpired claimsSet@JWTClaimsSet{ nbf } =
    case nbf of
        (Just expiredTime) -> do
            diffTime <- diffUTCTime ((posixSecondsToUTCTime . secondsSinceEpoch) expiredTime) <$> getCurrentTime
            pure $ if diffTime > 0 then Just claimsSet else Nothing
        Nothing -> pure Nothing

verifiedToken :: Text -> Maybe (JWT VerifiedJWT)
verifiedToken header = tokenFromHeader >>= \token -> decodeAndVerifySignature (secret secretKey) (toStrict token)
    where
        tokenFromHeader = case splitOn "=" header of
            [x, y] -> Just y
            _ -> Nothing

redirectToLogin :: Request -> Response -> Response
redirectToLogin Request{ rawPathInfo } response =
    if rawPathInfo == "/login" || rawPathInfo == "/signin" then response else
        ((mapResponseStatus $ const status302) .
        (mapResponseHeaders $ (:) ("Location", (encodeUtf8 . toStrict) "/login"))) response

redirectFromLogin :: Request -> Response -> Response
redirectFromLogin Request{ rawPathInfo } response =
    if rawPathInfo /= "/login" && rawPathInfo /= "/signin" then response else
        ((mapResponseStatus $ const status302) .
        (mapResponseHeaders $ (:) ("Location", "/"))) response

authMiddleware :: Middleware
authMiddleware app req@(Request{ requestHeaders, pathInfo }) response =
    let cookieHeader = find (\(headerName, _) -> headerName == "Cookie") requestHeaders in
        do
            token <- pure (
                do
                    (_, headerValue) <- cookieHeader
                    verified <- verifiedToken $ (fromStrict . decodeUtf8) headerValue
                    Just verified
                )
            notExpiredToken <- (
                do
                    case token of
                        (Just verified) -> checkExpired $ claims verified
                        Nothing -> pure $ Nothing
                )
            case notExpiredToken of
                (Just _) -> (putStrLn $ show req) >> app req (response . (redirectFromLogin req))
                Nothing -> app req (response . (redirectToLogin req))
