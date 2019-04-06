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
import qualified RestHandlers.Utils as Utils
import qualified Db

data SignInRequest = SignInRequest
  { email :: T.Text
  , password :: T.Text
  }

instance Yaml.FromJSON SignInRequest where
  parseJSON (Yaml.Object v) = SignInRequest
    <$> v .: "email"
    <*> v .: "password"

data SignInError =
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

errorToStatus :: SignInError -> Scotty.ActionM ()  
errorToStatus EmailAlreadyExists = Utils.makeErrorResponse 422 $ Just "Email already exists"
errorToStatus EmailLengthIncorrect = Utils.makeErrorResponse 422 $ Just emailErrorMessage
errorToStatus PasswordLengthIncorrect = Utils.makeErrorResponse 422 $ Just passwordLengthErrorMessage
errorToStatus InternalError = Utils.makeInternalErrorResponse

validatePasswordLength :: T.Text -> Either SignInError T.Text
validatePasswordLength password =
  if minPasswordLength <= l && l <= maxPasswordLength
  then Right password
  else Left PasswordLengthIncorrect
    where l = T.length password

validateEmailLength :: T.Text -> Either SignInError T.Text
validateEmailLength email =
  if T.length email <= maxEmailLength then Right email else Left EmailLengthIncorrect

validateEmail :: PSQL.Connection -> T.Text -> IO (Either SignInError T.Text)
validateEmail dbConn email = checkIfEmpty <$> Db.getUserCredsByEmail dbConn email
  where
    checkIfEmpty rows = case length rows of
      0 -> validateEmailLength email
      _ -> Left EmailAlreadyExists

validate :: PSQL.Connection -> SignInRequest -> IO (Either SignInError SignInRequest)
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
        [x] -> Utils.makeSuccessResponse
        _ -> Utils.makeInternalErrorResponse
    (Left err) -> errorToStatus err
