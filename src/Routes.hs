{-# LANGUAGE OverloadedStrings #-}

module Routes (routes) where

import Web.Scotty (get, html, ScottyM, notFound, middleware)
import Lucid (renderText)
import Data.Monoid (mconcat)
import Network.Wai.Middleware.Static (static)

import Views.LoginPage
import Views.NotFound

routes :: ScottyM ()
routes = do
    middleware static
    get "/login" $ html . renderText $ loginPageView True
    get "/signin" $ html . renderText $ loginPageView False
    notFound $ html . renderText $ notFoundView
