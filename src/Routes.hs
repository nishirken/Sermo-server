{-# LANGUAGE OverloadedStrings #-}

module Routes (routes) where

import Web.Scotty (get, post, html, ScottyM, notFound, middleware)
import Lucid (renderText)
import Data.Monoid (mconcat)
import Network.Wai.Middleware.Static (static)
import Database.PostgreSQL.Simple (Connection)

import Controllers.Login
import Controllers.Signin
import Views.LoginPage
import Views.SigninPage
import Views.NotFound
import Views.AppPage
import Models

renderHtml = html . renderText

routes :: Connection -> ScottyM ()
routes dbConn = do
    middleware static
    get "/" $ renderHtml $ appPageView
    get "/login" $ renderHtml $ loginPageView $ FormPageView ""
    get "/signin" $ renderHtml $ signinPageView $ FormPageView ""
    post "/login" $ loginController
    post "/signin" $ signinController dbConn
    notFound $ renderHtml $ notFoundView
