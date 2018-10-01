{-# LANGUAGE OverloadedStrings #-}

module Controllers.Signin where

import Data.Text.Lazy (pack, Text)
import qualified Data.Text.Lazy as T
import Web.Scotty (html, ActionM, param, text, redirect, rescue)
import Database.PostgreSQL.Simple (Connection, execute)
import Control.Monad.IO.Class (liftIO)
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

validate :: Signin -> Either Text Signin
validate (Signin email' password' repeatedPassword') =
    validateEmailLength email' >>= \email ->
        validatePasswordMatch password' repeatedPassword' >>= validatePasswordLength >>=
            \password -> Right $ Signin email password password

getParam :: Text -> ActionM Text
getParam paramName = do
    param paramName `rescue`
        \errorMessage -> return errorMessage

signinController :: Connection -> ActionM ()
signinController dbConn = do
    email <- getParam "email" :: ActionM Text
    password <- getParam "password"
    repeatedPassword <- getParam "repeatedPassword"
    case (validate $ Signin email password repeatedPassword) of
        (Right _) -> (liftIO $ execute dbConn "insert into users values (?,?)" (email, password)) >> redirect "/"
        (Left errorMessage) -> html . renderText $ signinPageView $ FormPageView errorMessage
