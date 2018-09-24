{-# LANGUAGE OverloadedStrings #-}

module Routes (routes) where

import Web.Scotty (get, html, ScottyM, notFound)
import Lucid (renderText)
import Data.Monoid (mconcat)
import Controllers.Login
import Controllers.NotFound

routes :: ScottyM ()
routes = do
  get "/" $ loginController
  notFound $ notFoundController
