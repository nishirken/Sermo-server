{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Routes (routes) where

import Web.Scotty (get, post, html, ScottyM, notFound, middleware)
import Lucid (renderText)
import Data.Monoid (mconcat)
import Network.Wai.Middleware.Static (static)
import Database.PostgreSQL.Simple (Connection)

import Controllers.LogIn
import Controllers.SignIn
import Controllers.Auth (authMiddleware)
import Controllers.LogOut
import Views.LogInPage
import Views.SignInPage
import Views.NotFound
import Views.AppPage
import Models
import Config (Config (..))

renderHtml = html . renderText

routes :: Connection -> Config -> ScottyM ()
routes dbConn Config{ authKey }  = do
    middleware static
    middleware $ authMiddleware authKey
    get "/" $ renderHtml $ appPageView
    get "/login" $ renderHtml $ logInPageView $ FormPageView ""
    get "/signin" $ renderHtml $ signInPageView $ FormPageView ""
    post "/login" $ logInController authKey dbConn
    post "/signin" $ signInController authKey dbConn
    get "/logout" $ logOutController
    notFound $ renderHtml $ notFoundView
