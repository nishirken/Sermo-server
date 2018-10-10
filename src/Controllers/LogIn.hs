{-# LANGUAGE OverloadedStrings #-}

module Controllers.LogIn where

import Data.Text.Lazy (Text, pack, unpack, toStrict)
import Data.Text.Encoding (encodeUtf8)
import Control.Monad.IO.Class (liftIO)
import Web.Scotty (ActionM, addHeader, html, redirect)
import Lucid (renderText)
import Database.PostgreSQL.Simple (Connection)
import Controllers.Utils (getParam)
import Controllers.Auth (createToken)
import Data.Either (either)
import Control.Monad.Except (ExceptT (..), runExceptT)
import Views.LogInPage (logInPageView)
import Models (FormPageView (..))
import Db (getUserByEmail)
import Crypto.Scrypt (defaultParams, EncryptedPass (..), Pass (..), verifyPass)

errorMessage = "Incorrect password or email"

verify :: Text -> Text -> Bool
verify password passwordHash =
    fst $ verifyPass
        defaultParams
        ((Pass . encodeUtf8 . toStrict) password)
        ((EncryptedPass . encodeUtf8 . toStrict) passwordHash)

validate :: Connection -> Text -> Text -> ExceptT Text IO Int
validate dbConn email password = ExceptT $ getUserByEmail dbConn email >>= \rows ->
    case (rows) of
        [] -> pure $ Left errorMessage
        [(id, email', password')] -> pure $ if verify password password' then Right id else Left errorMessage

makeTokenHeader :: Text -> Int -> IO Text
makeTokenHeader authKey userId = do
    token <- createToken authKey $ (pack . show) userId
    pure $ "token=" <> token

logInController :: Text -> Connection -> ActionM ()
logInController authKey dbConn = do
    email <- getParam "email"
    password <- getParam "password"
    validated <- liftIO . runExceptT $ validate dbConn email password
    case (validated) of
        (Right userId) -> do
            token <- liftIO $ makeTokenHeader authKey userId
            addHeader "Set-Cookie" token
            redirect "/"
        (Left errorMessage) -> html . renderText $ logInPageView $ FormPageView errorMessage
