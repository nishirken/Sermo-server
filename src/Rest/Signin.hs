{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Rest.Signin where

import qualified Data.Text as T
import qualified Web.Scotty as Scotty
import qualified Database.PostgreSQL.Simple as PSQL
import qualified Network.HTTP.Types as NetworkTypes
import qualified Data.Yaml as Yaml
import Control.Monad.IO.Class (liftIO)
import qualified Rest.Utils as Utils
import Models (LoginRequest (..))
import Models.TokenObject (TokenObject (..))
import qualified Db
import qualified Rest.Auth as Auth

data SigninError =
  EmailAlreadyExists
  | EmailLengthIncorrect
  | PasswordLengthIncorrect
  | InternalError
  deriving (Eq)

minPasswordLength = 6
maxPasswordLength = 20

passwordLengthErrorMessage = T.pack $
  "Password length incorrect, min length is " <>
  show minPasswordLength <>
  ", max is " <>
  show maxPasswordLength

maxEmailLength = 50

emailErrorMessage = T.pack $ "Email length incorrect, max length is " <> show maxEmailLength

errorToStatus :: SigninError -> Scotty.ActionM ()
errorToStatus EmailAlreadyExists = Utils.makeErrorResponse 422 $ Just "Email already exists"
errorToStatus EmailLengthIncorrect = Utils.makeErrorResponse 422 $ Just emailErrorMessage
errorToStatus PasswordLengthIncorrect = Utils.makeErrorResponse 422 $ Just passwordLengthErrorMessage
errorToStatus InternalError = Utils.makeInternalErrorResponse

validatePasswordLength :: T.Text -> Either SigninError T.Text
validatePasswordLength password =
  if minPasswordLength <= l && l <= maxPasswordLength
  then Right password
  else Left PasswordLengthIncorrect
    where l = T.length password

validateEmailLength :: T.Text -> Either SigninError T.Text
validateEmailLength email =
  if T.length email <= maxEmailLength then Right email else Left EmailLengthIncorrect

validateEmail :: PSQL.Connection -> T.Text -> IO (Either SigninError T.Text)
validateEmail dbConn email = checkIfEmpty <$> Db.getUserCredsByEmail dbConn email
  where
    checkIfEmpty rows = case length rows of
      0 -> validateEmailLength email
      _ -> Left EmailAlreadyExists

validate :: PSQL.Connection -> LoginRequest -> IO (Either SigninError LoginRequest)
validate dbConn (LoginRequest _email _password) = do
  validatedEmailEither <- validateEmail dbConn _email
  pure $ do
    email <- validatedEmailEither
    password <- validatePasswordLength _password
    pure $ LoginRequest email password

signinHandler :: T.Text -> PSQL.Connection -> Scotty.ActionM ()
signinHandler authKey dbConn = do
  request <- Scotty.jsonData :: Scotty.ActionM LoginRequest
  validated <- liftIO $ validate dbConn request
  case validated of
    (Right LoginRequest { _email, _password }) -> do
      userId <- liftIO $ Db.setUser dbConn _email _password
      case userId of
        [x] -> do
          token <- liftIO $ Auth.createToken authKey $ (T.pack . show) x
          Utils.makeDataResponse $ TokenObject token
        _ -> Utils.makeInternalErrorResponse
    (Left err) -> errorToStatus err
