{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module RestHandlers.SignIn where

import qualified Data.Text as T
import qualified Web.Scotty as Scotty
import qualified Database.PostgreSQL.Simple as PSQL
import qualified Network.HTTP.Types as NetworkTypes
import qualified Data.Yaml as Yaml
import Data.Yaml ((.:))
import Control.Monad.IO.Class (liftIO)

import qualified Db
import RestHandlers.Utils (makeStatus, internalErrorStatus, successResponse)
import RestHandlers.Types (MapError, errorToStatus)

data SignInRequest = SignInRequest {
  email :: T.Text
  , password :: T.Text
}

instance Yaml.FromJSON SignInRequest where
  parseJSON (Yaml.Object v) = SignInRequest
    <$> v .: "email"
    <*> v .: "password"

data SingInError =
  EmailAlreadyExists
  | EmailLengthIncorrect
  | PasswordLengthIncorrect
  | InternalError
  deriving (Eq)

instance MapError SingInError where
  errorToStatus EmailAlreadyExists = makeStatus 422 "Email already exists"
  errorToStatus EmailLengthIncorrect = makeStatus 422 emailErrorMessage
  errorToStatus PasswordLengthIncorrect = makeStatus 422 passwordLengthErrorMessage
  errorToStatus _ = internalErrorStatus

minPasswordLength = 6
maxPasswordLength = 20

passwordLengthErrorMessage = T.pack $
  "Password length incorrect, min length is " <>
  show minPasswordLength <>
  ", max is " <>
  show maxPasswordLength

maxEmailLength = 50

emailErrorMessage = T.pack $ "Email length incorrect, max length is " <> show maxEmailLength

validatePasswordLength :: T.Text -> Either SingInError T.Text
validatePasswordLength password =
  if minPasswordLength <= l && l <= maxPasswordLength
  then Right password
  else Left PasswordLengthIncorrect
    where l = T.length password

validateEmailLength :: T.Text -> Either SingInError T.Text
validateEmailLength email =
  if T.length email <= maxEmailLength then Right email else Left EmailLengthIncorrect

validateEmail :: PSQL.Connection -> T.Text -> IO (Either SingInError T.Text)
validateEmail dbConn email = checkIfEmpty <$> Db.getUserCredsByEmail dbConn email
  where
    checkIfEmpty rows = case length rows of
      0 -> validateEmailLength email
      _ -> Left EmailAlreadyExists

validate :: PSQL.Connection -> SignInRequest -> IO (Either SingInError SignInRequest)
validate dbConn (SignInRequest email' password') = do
  validatedEmailEither <- validateEmail dbConn email'
  pure $ do
    email <- validatedEmailEither
    password <- validatePasswordLength password'
    pure $ SignInRequest email password

signInHandler :: T.Text -> PSQL.Connection -> Scotty.ActionM ()
signInHandler authKey dbConn = do
  request <- Scotty.jsonData :: Scotty.ActionM SignInRequest
  validated <- liftIO $ validate dbConn request
  case validated of
    (Right SignInRequest { email, password }) -> do
      userId <- liftIO $ Db.setUser dbConn email password
      case userId of
        [x] -> successResponse
        _ -> internalErrorStatus
    (Left err) -> errorToStatus err
