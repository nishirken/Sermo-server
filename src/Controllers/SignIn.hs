{-# LANGUAGE OverloadedStrings #-}

module Controllers.SignIn where

import Data.Text.Lazy (pack, Text)
import qualified Data.Text.Lazy as T
import Web.Scotty (html, ActionM, param, text, redirect, rescue)
import Database.PostgreSQL.Simple (Connection, execute, Only (..), query)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Except (ExceptT (..), runExceptT, liftEither)
import Lucid (renderText)
import Views.SignInPage (signInPageView)
import Db (getUserByEmail)
import Models
import Validation (validateEmailLength, validatePasswordMatch, validatePasswordLength)

validateEmail :: Connection -> Text -> ExceptT Text IO Text
validateEmail dbConn email =
    ExceptT $ fmap checkIfEmpty $ getUserByEmail dbConn email
        where
            checkIfEmpty rows = case (length rows) of
                0 -> validateEmailLength email
                _ -> Left $ pack "Email already exists"

validate :: Connection -> SignIn -> ExceptT Text IO SignIn
validate dbConn (SignIn email' password' repeatedPassword') =
    validateEmail dbConn email' >>= \email -> ExceptT $ return $
        validatePasswordMatch password' repeatedPassword' >>= validatePasswordLength >>= \password ->
            Right $ SignIn email password password

getParam :: Text -> ActionM Text
getParam paramName = do
    param paramName `rescue`
        \errorMessage -> return errorMessage

signInController :: Connection -> ActionM ()
signInController dbConn = do
    email <- getParam "email" :: ActionM Text
    password <- getParam "password"
    repeatedPassword <- getParam "repeatedPassword"
    validated <- liftIO . runExceptT $ validate dbConn $ SignIn email password repeatedPassword
    case (validated) of
        (Right _) -> (liftIO $ execute dbConn "insert into users values (?,?)" (email, password)) >> redirect "/"
        (Left errorMessage) -> html . renderText $ signInPageView $ FormPageView errorMessage
