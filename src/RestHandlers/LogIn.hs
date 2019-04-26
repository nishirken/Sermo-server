{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module RestHandlers.LogIn where

import qualified Data.Text as T
import qualified Data.Text.Encoding as TEncoding
import qualified Web.Scotty as Scotty
import qualified Database.PostgreSQL.Simple as PSQL
import qualified Crypto.Scrypt as Crypto
import qualified Data.Yaml as Yaml
import Data.Yaml ((.:), (.=))
import Data.Either (either)
import Control.Monad.IO.Class (liftIO)

import qualified Db
import qualified RestHandlers.Utils as Utils
import RestHandlers.Auth (createToken)

data LogInRequest = LogInRequest
    { email :: T.Text
    , password :: T.Text
    }

instance Yaml.FromJSON LogInRequest where
    parseJSON (Yaml.Object v) = LogInRequest
        <$> v .: "email"
        <*> v .: "password"

data LogInError =
    IncorrectPassword
    | IncorrectEmail
    | InternalError
    deriving (Eq)

badCredsStatus = Utils.makeErrorResponse 422 $ Just "Incorrect password or email"

errorToStatus :: LogInError -> Scotty.ActionM ()
errorToStatus IncorrectPassword = badCredsStatus
errorToStatus IncorrectEmail = badCredsStatus
errorToStatus _ = Utils.makeInternalErrorResponse

verify :: T.Text -> T.Text -> Bool
verify password passwordHash =
    fst $ Crypto.verifyPass
        Crypto.defaultParams
        ((Crypto.Pass . TEncoding.encodeUtf8) password)
        ((Crypto.EncryptedPass . TEncoding.encodeUtf8) passwordHash)

validate :: PSQL.Connection -> T.Text -> T.Text -> IO (Either LogInError Int)
validate dbConn email password = do
    rows <- Db.getUserCredsByEmail dbConn email
    case rows of
        [(id, email', password')] ->
            pure $ if verify password password' then Right id else Left IncorrectPassword
        [] -> pure $ Left IncorrectEmail
        _ -> pure $ Left InternalError

logInHandler :: T.Text -> PSQL.Connection -> Scotty.ActionM ()
logInHandler authKey dbConn = do
    LogInRequest { email, password } <- Scotty.jsonData :: Scotty.ActionM LogInRequest
    validated <- liftIO $ validate dbConn email password
    case validated of
        (Right userId) -> do
            token <- liftIO $ createToken authKey $ (T.pack . show) userId
            Utils.makeDataResponse $ Utils.TokenResponse token
        (Left err) -> errorToStatus err
