{-# LANGUAGE OverloadedStrings #-}

module Controllers.LogIn where

import Data.Text.Lazy (Text, pack, unpack)
import Data.Text.Encoding (encodeUtf8)
import Control.Monad.IO.Class (liftIO)
import Web.Scotty (ActionM, addHeader, html, redirect)
import Lucid (renderText)
import Database.PostgreSQL.Simple (Connection)
import Controllers.Utils (getParam)
import Controllers.JWT (createToken)
import Data.Either (either)
import Control.Monad.Except (ExceptT (..), runExceptT)
import Views.LogInPage (logInPageView)
import Models (FormPageView (..))
import Validation (validatePasswordMatch)
import Db (getUserByEmail)

errorMessage = "Incorrect password or email"

validate :: Connection -> Text -> Text -> ExceptT Text IO Text
validate dbConn email password = ExceptT $ getUserByEmail dbConn email >>= \rows ->
    case (rows) of
        [] -> pure $ Left errorMessage
        [(email', password')] -> pure $ validatePasswordMatch password password'

makeTokenHeader :: Text -> IO Text
makeTokenHeader email = do
    token <- createToken email
    pure $ "token=" <> token

logInController :: Connection -> ActionM ()
logInController dbConn = do
    email <- getParam "email"
    password <- getParam "password"
    validated <- liftIO . runExceptT $ validate dbConn email password
    case (validated) of
        (Right _) -> do
            token <- liftIO $ makeTokenHeader email
            addHeader "Set-Cookie" token
            redirect "/"
        (Left errorMessage) -> html . renderText $ logInPageView $ FormPageView errorMessage
