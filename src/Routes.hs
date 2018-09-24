{-# LANGUAGE OverloadedStrings #-}

module Routes (routes) where

import Web.Scotty (get, html, ScottyM)

import Data.Monoid (mconcat)

routes :: ScottyM ()
routes = do
  get "/" $ html $ mconcat ["<h1>Hello world</h1>"]