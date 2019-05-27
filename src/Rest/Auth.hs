{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Rest.Auth (isTokenValid, createToken, isAuthorizedHandler) where

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
    , StringOrURI
    , encodeSigned
    , numericDate
    , secondsSinceEpoch
    , secret
    , stringOrURI
    , stringOrURIToText
    , iss
    , jti
    , nbf
    , JSON
    , decode
    )
import Network.Wai (Middleware, mapResponseHeaders, mapResponseStatus)
import Network.Wai.Internal (Request (..), Response (..))
import Network.HTTP.Types (status401)
import Models (SuccessResponse (..))
import Models.TokenObject (TokenObject (..))
import Web.Scotty (ActionM, jsonData)
import Control.Monad.IO.Class (liftIO)
import qualified Database.PostgreSQL.Simple as PSQL
import qualified Data.Yaml as Yaml
import Data.Yaml ((.:))
import qualified Utils
import qualified Db
import Data.Text.Read (decimal)

expirationTime :: IO NominalDiffTime
expirationTime = utcTimeToPOSIXSeconds . addUTCTime (600 * 60 :: NominalDiffTime) <$> getCurrentTime

createToken :: T.Text -> T.Text -> IO T.Text
createToken secretKey userId = do
  expiration <- expirationTime
  pure $ encodeSigned HS256 (secret secretKey) def {
    iss = stringOrURI "hs-auth"
    , jti = stringOrURI userId
    , nbf = numericDate expiration
  }

isTokenIdValid :: JWT VerifiedJWT -> PSQL.Connection -> IO Bool
isTokenIdValid token conn = let JWTClaimsSet { jti } = claims token in case toInt jti of
  (Just parsedIntJti) -> do
    rows <- Db.getUserCredsById conn parsedIntJti
    pure $ not (null rows)
  Nothing -> pure False
  where
    toInt :: Maybe StringOrURI -> Maybe Int
    toInt stringJti = do
      justJti <- stringJti
      let
        textJti = stringOrURIToText justJti
        rightToMaybe eitherValue = case eitherValue of
          (Left _) -> Nothing
          (Right x) -> Just x
      parsedJti <- rightToMaybe $ decimal textJti
      pure $ fst parsedJti

isTokenTimeValid :: JWT VerifiedJWT -> IO Bool
isTokenTimeValid token = let JWTClaimsSet { nbf } = claims token in
  case nbf of
    (Just expiredTime) -> do
      diffTime <- diffUTCTime ((posixSecondsToUTCTime . secondsSinceEpoch) expiredTime) <$> getCurrentTime
      pure $ diffTime > 0
    Nothing -> pure False

isTokenValid :: T.Text -> T.Text -> PSQL.Connection -> IO Bool
isTokenValid authKey token dbConn =
  case decodeAndVerifySignature (secret authKey) token of
    (Just verified) -> do
      idRes <- isTokenIdValid verified dbConn
      expiredRes <- isTokenTimeValid verified
      pure $ idRes && expiredRes
    Nothing -> pure False

isAuthorizedHandler :: T.Text -> PSQL.Connection -> ActionM ()
isAuthorizedHandler authKey conn = do
  TokenObject { _token } <- jsonData :: ActionM TokenObject
  isValid <- liftIO $ isTokenValid authKey _token conn
  Utils.makeDataResponse $ SuccessResponse isValid
