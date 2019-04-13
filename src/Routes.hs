{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Routes (routes) where

import Web.Scotty (get, post, html, ScottyM, notFound, middleware, status, json, jsonData, ActionM)
import Network.Wai.Middleware.Static (static)
import Network.Wai.Middleware.Cors (cors, CorsResourcePolicy (..))
import Database.PostgreSQL.Simple (Connection)
import RestHandlers.LogIn
import RestHandlers.SignIn
import RestHandlers.Auth (isAuthorizedHandler)
import qualified Data.Yaml as Yaml
import qualified RestHandlers.Utils as Utils
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text as T
import Data.CaseInsensitive (mk)
import Config (Config (..))
import Main.Schemas (graphqlHandler)
import Control.Monad.IO.Class (liftIO)

corsConfig = CorsResourcePolicy {
  corsOrigins = Nothing
  , corsMethods = encodeUtf8 <$> ["POST", "GET"]
  , corsRequestHeaders = mk . encodeUtf8 <$> ["Accept", "Accept-Language", "Content-Language", "Content-Type"]
  , corsExposedHeaders = Just $ mk . encodeUtf8 <$> ["Access-Control-Allow-Origin"]
  , corsMaxAge = Nothing
  , corsVaryOrigin = False
  , corsRequireOrigin = True
  , corsIgnoreFailures = False
}

routes :: Connection -> Config -> ScottyM ()
routes dbConn Config{ authKey } = do
  middleware static
  middleware $ cors $ \_ -> Just corsConfig
  post "/login" $ logInHandler authKey dbConn
  post "/signin" $ signInHandler authKey dbConn
  post "/graphql" $ graphqlHandler dbConn authKey
  post "/auth" $ isAuthorizedHandler authKey dbConn
  notFound $ Utils.makeErrorResponse 404 $ Just "Method not found"
