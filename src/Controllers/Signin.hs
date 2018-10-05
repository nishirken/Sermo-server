{-# LANGUAGE OverloadedStrings #-}

module Controllers.Signin where

import Data.Text.Lazy (pack, Text)
import qualified Data.Text.Lazy as T
import Web.Scotty (html, ActionM, param, text, redirect, rescue)
import Database.PostgreSQL.Simple (Connection, execute, Only (..), query)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Except (ExceptT (..), runExceptT, liftEither)
import Lucid (renderText)
import Views.SigninPage (signinPageView)
import Models

validatePasswordMatch :: Text -> Text -> Either Text Text
validatePasswordMatch pass repeated =
    if pass == repeated then Right pass else Left "Passwords doesn't match"

validatePasswordLength :: Text -> Either Text Text
validatePasswordLength password =
    if and [l < 21, l > 5] then Right password else Left "Password length incorrect, min length is 6, max is 20"
        where l = T.length password

validateEmailLength :: Text -> Either Text Text
validateEmailLength email =
    if T.length email < 51 then Right email else Left "Email length incorrect, max length is 50"

getUserCredsByEmail :: Connection -> Text -> IO [(Text, Text)]
getUserCredsByEmail dbConn email = query dbConn "select email, password from users where email = ?" (Only email)

validateEmail :: Connection -> Text -> ExceptT Text IO Text
validateEmail dbConn email =
    ExceptT $ fmap checkIfEmpty $ getUserCredsByEmail dbConn email
        where
            checkIfEmpty rows = case (length rows) of
                0 -> validateEmailLength email
                _ -> Left $ pack "Email already exists"

validate :: Connection -> Signin -> ExceptT Text IO Signin
validate dbConn (Signin email' password' repeatedPassword') =
    validateEmail dbConn email' >>= \email -> ExceptT $ return $
        validatePasswordMatch password' repeatedPassword' >>= validatePasswordLength >>= \password ->
            Right $ Signin email password password

getParam :: Text -> ActionM Text
getParam paramName = do
    param paramName `rescue`
        \errorMessage -> return errorMessage

signinController :: Connection -> ActionM ()
signinController dbConn = do
    email <- getParam "email" :: ActionM Text
    password <- getParam "password"
    repeatedPassword <- getParam "repeatedPassword"
    validated <- (liftIO . runExceptT) $ validate dbConn $ Signin email password repeatedPassword
    case (validated) of
        (Right _) -> (liftIO $ execute dbConn "insert into users values (?,?)" (email, password)) >> redirect "/"
        (Left errorMessage) -> html . renderText $ signinPageView $ FormPageView errorMessage
