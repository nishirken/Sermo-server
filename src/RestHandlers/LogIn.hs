{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module RestHandlers.Login where

import qualified Data.Text as T
import qualified Data.Text.Encoding as TEncoding
import qualified Web.Scotty as Scotty
import qualified Database.PostgreSQL.Simple as PSQL
import qualified Crypto.Scrypt as Crypto
import qualified Data.Yaml as Yaml
import Data.Yaml ((.:), (.=))
import Data.Either (either)
import Control.Monad.IO.Class (liftIO)
import Models.Index (LoginRequest (..))
import Models.TokenObject (TokenObject (..))
import qualified Db
import qualified RestHandlers.Utils as Utils
import RestHandlers.Auth (createToken)

data LoginError =
  IncorrectPassword
  | IncorrectEmail
  | InternalError
  deriving (Eq)

badCredsStatus = Utils.makeErrorResponse 422 $ Just "Incorrect password or email"

errorToStatus :: LoginError -> Scotty.ActionM ()
errorToStatus IncorrectPassword = badCredsStatus
errorToStatus IncorrectEmail = badCredsStatus
errorToStatus _ = Utils.makeInternalErrorResponse

verify :: T.Text -> T.Text -> Bool
verify password passwordHash =
  fst $ Crypto.verifyPass
    Crypto.defaultParams
    ((Crypto.Pass . TEncoding.encodeUtf8) password)
    ((Crypto.EncryptedPass . TEncoding.encodeUtf8) passwordHash)

validate :: PSQL.Connection -> T.Text -> T.Text -> IO (Either LoginError Int)
validate dbConn email password = do
  rows <- Db.getUserCredsByEmail dbConn email
  case rows of
    [(id, email', password')] ->
        pure $ if verify password password' then Right id else Left IncorrectPassword
    [] -> pure $ Left IncorrectEmail
    _ -> pure $ Left InternalError

loginHandler :: T.Text -> PSQL.Connection -> Scotty.ActionM ()
loginHandler authKey dbConn = do
  LoginRequest { _email, _password } <- Scotty.jsonData :: Scotty.ActionM LoginRequest
  validated <- liftIO $ validate dbConn _email _password
  case validated of
    (Right userId) -> do
        token <- liftIO $ createToken authKey $ (T.pack . show) userId
        Utils.makeDataResponse $ TokenObject token
    (Left err) -> errorToStatus err
