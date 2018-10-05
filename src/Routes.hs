{-# LANGUAGE OverloadedStrings #-}

module Routes (routes) where

import Web.Scotty (get, post, html, ScottyM, notFound, middleware)
import Lucid (renderText)
import Data.Monoid (mconcat)
import Network.Wai.Middleware.Static (static)
import Database.PostgreSQL.Simple (Connection)

import Controllers.LogIn
import Controllers.SignIn
import Views.LogInPage
import Views.SignInPage
import Views.NotFound
import Views.AppPage
import Models

renderHtml = html . renderText

routes :: Connection -> ScottyM ()
routes dbConn = do
    middleware static
    get "/" $ renderHtml $ appPageView
    get "/login" $ renderHtml $ logInPageView $ FormPageView ""
    get "/SignIn" $ renderHtml $ signInPageView $ FormPageView ""
    post "/login" $ logInController
    post "/SignIn" $ signInController dbConn
    notFound $ renderHtml $ notFoundView
