{-# LANGUAGE OverloadedStrings #-}

module Controllers.SignIn where

import Data.Text.Lazy (pack, Text)
import qualified Data.Text.Lazy as T
import Web.Scotty (html, ActionM, text, redirect)
import Database.PostgreSQL.Simple (Connection, fromOnly)
import Control.Monad.IO.Class (liftIO)
import Lucid (renderText)
import Views.SignInPage (signInPageView)
import Db (getUserByEmail, setUser)
import Models
import Controllers.Utils (getParam)
import Controllers.LogIn (setAuthCookie)
import Validation (validateEmailLength, validatePasswordMatch, validatePasswordLength)

validateEmail :: Connection -> Text -> IO (Either Text Text)
validateEmail dbConn email = checkIfEmpty <$> getUserByEmail dbConn email
    where
        checkIfEmpty rows = case (length rows) of
            0 -> validateEmailLength email
            _ -> Left $ pack "Email already exists"

validate :: Connection -> SignIn -> IO (Either Text SignIn)
validate dbConn (SignIn email' password' repeatedPassword') = do
    eitherEmail <- validateEmail dbConn email'
    pure $ do
        email <- eitherEmail
        password <- validatePasswordMatch password' repeatedPassword' >>= validatePasswordLength
        pure $ SignIn email password password

signInController :: Text -> Connection -> ActionM ()
signInController authKey dbConn = do
    email <- getParam "email"
    password <- getParam "password"
    repeatedPassword <- getParam "repeatedPassword"
    validated <- liftIO $ validate dbConn $ SignIn email password repeatedPassword
    case validated of
        (Right _) -> do
            userId <- liftIO $ setUser dbConn email password
            case userId of
                [x] -> (setAuthCookie authKey $ fromOnly x) >> redirect "/"
                _ -> html . renderText $ signInPageView $ FormPageView "Internal error"
        (Left errorMessage) -> html . renderText $ signInPageView $ FormPageView errorMessage
