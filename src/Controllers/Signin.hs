{-# LANGUAGE OverloadedStrings #-}

module Controllers.Signin where

import Data.Text.Lazy (pack, Text)
import Web.Scotty (html, ActionM, param, text, redirect, rescue)
import Lucid (renderText)
import Views.SigninPage (signinPageView)
import Models

validatePassword :: Text -> Text -> Either Text Text
validatePassword pass repeated =
    if pass == repeated then Right pass else Left "Passwords doesn't match"

getParam :: Text -> ActionM Text
getParam paramName = do
    param paramName `rescue`
        \errorMessage -> return errorMessage

signinController :: ActionM ()
signinController = do
    email <- getParam "email" :: ActionM Text
    password <- getParam "password"
    repeatedPassword <- getParam "repeatedPassword"
    case (validatePassword password repeatedPassword) of
        (Right pass) -> redirect "/"
        (Left errorMessage) -> html . renderText $ signinPageView $ FormPageView errorMessage
