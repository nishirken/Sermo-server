{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Routes (routes) where

import Web.Scotty (get, post, html, ScottyM, notFound, middleware, status)
import Network.Wai.Middleware.Static (static)
import Network.Wai.Middleware.Cors (simpleCors)
import Database.PostgreSQL.Simple (Connection)
import Handlers.LogIn
import Handlers.SignIn
import Handlers.Auth (authMiddleware)
import Handlers.Utils (makeStatus)
import Config (Config (..))

routes :: Connection -> Config -> ScottyM ()
routes dbConn Config{ authKey }  = do
    middleware static
    middleware simpleCors
    middleware $ authMiddleware authKey
    post "/login" $ logInHandler authKey dbConn
    post "/signin" $ signInHandler authKey dbConn
    notFound $ makeStatus 404 "Method not found"
