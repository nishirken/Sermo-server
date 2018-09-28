{-# LANGUAGE OverloadedStrings #-}

module Routes (routes) where

import Web.Scotty (get, html, ScottyM, notFound, middleware)
import Lucid (renderText)
import Data.Monoid (mconcat)
import Controllers.Login
import Controllers.NotFound
import Network.Wai.Middleware.Static (static)

routes :: ScottyM ()
routes = do
    middleware static
    get "/" $ loginController
    notFound $ notFoundController
