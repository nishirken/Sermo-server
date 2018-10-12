{-# LANGUAGE OverloadedStrings #-}

module Controllers.SignIn where

import Data.Text.Lazy (pack, Text)
import qualified Data.Text.Lazy as T
import Web.Scotty (html, ActionM, text, redirect)
import Database.PostgreSQL.Simple (Connection, execute, Only (..), query)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Except (ExceptT (..), runExceptT, liftEither)
import Lucid (renderText)
import Views.SignInPage (signInPageView)
import Db (getUserByEmail, setUser)
import Models
import Controllers.Utils (getParam)
import Controllers.LogIn (setAuthCookie)
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

signInController :: Text -> Connection -> ActionM ()
signInController authKey dbConn = do
    email <- getParam "email"
    password <- getParam "password"
    repeatedPassword <- getParam "repeatedPassword"
    validated <- liftIO . runExceptT $ validate dbConn $ SignIn email password repeatedPassword
    userId <- setUser dbConn email password
    setAuthCookie authKey 2
    redirect "/"
