{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Handlers.LogIn where

import qualified Data.Text.Lazy as TLazy
import qualified Data.Text.Encoding as TEncoding
import qualified Web.Scotty as Scotty
import qualified Database.PostgreSQL.Simple as PSQL
import qualified Crypto.Scrypt as Crypto
import qualified Data.Yaml as Yaml
import Data.Yaml ((.:), (.=))
import Data.Either (either)
import Control.Monad.IO.Class (liftIO)

import qualified Db
import Handlers.Utils (getParam, makeStatus, internalErrorStatus)
import Handlers.Types (MapError, errorToStatus)
import Handlers.Crypt (createToken)

data LogInRequest = LogInRequest {
    email :: TLazy.Text
    , password :: TLazy.Text
}

instance Yaml.FromJSON LogInRequest where
    parseJSON (Yaml.Object v) = LogInRequest
        <$> v .: "email"
        <*> v .: "password"

newtype LogInResponse = LogInResponse { token :: TLazy.Text }

instance Yaml.ToJSON LogInResponse where
    toJSON (LogInResponse token) = Yaml.object ["token" .= token]

data LogInError =
    IncorrectPassword
    | IncorrectEmail
    | InternalError
    deriving (Eq)

badCredsStatus = makeStatus 422 "Incorrect password or email"

instance MapError LogInError where
    errorToStatus IncorrectPassword = badCredsStatus
    errorToStatus IncorrectEmail = badCredsStatus
    errorToStatus _ = internalErrorStatus

verify :: TLazy.Text -> TLazy.Text -> Bool
verify password passwordHash =
    fst $ Crypto.verifyPass
        Crypto.defaultParams
        ((Crypto.Pass . TEncoding.encodeUtf8 . TLazy.toStrict) password)
        ((Crypto.EncryptedPass . TEncoding.encodeUtf8 . TLazy.toStrict) passwordHash)

validate :: PSQL.Connection -> TLazy.Text -> TLazy.Text -> IO (Either LogInError Int)
validate dbConn email password = do
    rows <- Db.getUserByEmail dbConn email
    case rows of
        [(id, email', password')] ->
            pure $ if verify password password' then Right id else Left IncorrectPassword
        [] -> pure $ Left IncorrectEmail
        _ -> pure $ Left InternalError

logInHandler :: TLazy.Text -> PSQL.Connection -> Scotty.ActionM ()
logInHandler authKey dbConn = do
    LogInRequest { email, password } <- Scotty.jsonData :: Scotty.ActionM LogInRequest
    validated <- liftIO $ validate dbConn email password
    case validated of
        (Right userId) -> do
            token <- liftIO $ createToken authKey $ (TLazy.pack . show) userId
            Scotty.json $ LogInResponse token
        (Left err) -> errorToStatus err
