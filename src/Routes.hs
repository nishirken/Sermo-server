{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Routes (routes) where

import Web.Scotty (get, post, html, ScottyM, notFound, middleware, status)
import Network.Wai.Middleware.Static (static)
import Network.Wai.Middleware.Cors (cors, CorsResourcePolicy (..))
import Database.PostgreSQL.Simple (Connection)
import Handlers.LogIn
import Handlers.SignIn
import Handlers.Auth (authMiddleware)
import Handlers.Utils (makeStatus)
import Data.Text.Encoding (encodeUtf8)
import Data.CaseInsensitive (mk)
import Config (Config (..))

corsConfig = CorsResourcePolicy {
  corsOrigins = Nothing
  , corsMethods = encodeUtf8 <$> ["POST", "GET"]
  , corsRequestHeaders = (mk . encodeUtf8) <$> ["Accept", "Accept-Language", "Content-Language", "Content-Type"]
  , corsExposedHeaders = Just $ (mk . encodeUtf8) <$> ["Access-Control-Allow-Origin"]
  , corsMaxAge = Nothing
  , corsVaryOrigin = False
  , corsRequireOrigin = True
  , corsIgnoreFailures = False
}

routes :: Connection -> Config -> ScottyM ()
routes dbConn Config{ authKey }  = do
    middleware static
    middleware $ cors $ \_ -> Just corsConfig
    middleware $ authMiddleware authKey
    post "/login" $ logInHandler authKey dbConn
    post "/signin" $ signInHandler authKey dbConn
    notFound $ makeStatus 404 "Method not found"
