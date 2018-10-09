{-# LANGUAGE OverloadedStrings #-}

module Routes (routes) where

import Web.Scotty (get, post, html, ScottyM, notFound, middleware)
import Lucid (renderText)
import Data.Monoid (mconcat)
import Network.Wai.Middleware.Static (static)
import Database.PostgreSQL.Simple (Connection)

import Controllers.LogIn
import Controllers.SignIn
import Controllers.Auth (authMiddleware)
import Views.LogInPage
import Views.SignInPage
import Views.NotFound
import Views.AppPage
import Models

renderHtml = html . renderText

routes :: Connection -> ScottyM ()
routes dbConn = do
    middleware static
    middleware authMiddleware
    get "/" $ renderHtml $ appPageView
    get "/login" $ renderHtml $ logInPageView $ FormPageView ""
    get "/signin" $ renderHtml $ signInPageView $ FormPageView ""
    post "/login" $ logInController dbConn
    post "/signin" $ signInController dbConn
    notFound $ renderHtml $ notFoundView
