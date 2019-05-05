{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Routes (routes, middlewares) where

import Web.Scotty (get, post, html, ScottyM, notFound, middleware, status, json, jsonData, ActionM)
import Network.Wai.Middleware.Static (static)
import Network.Wai.Middleware.Cors (cors, CorsResourcePolicy (..))
import Database.PostgreSQL.Simple (Connection)
import Rest.Login (loginHandler)
import Rest.Signin (signinHandler)
import Rest.Auth (isAuthorizedHandler)
import qualified Data.Yaml as Yaml
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text as T
import Data.CaseInsensitive (mk)
import Config (Config (..))
import qualified Utils
import Main.Schemas (interpretUserQuery)
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

middlewares :: ScottyM ()
middlewares = do
  middleware static
  middleware $ cors $ \_ -> Just corsConfig

routes :: Connection -> Config -> ScottyM ()
routes dbConn Config{ authKey } = do
  post "/login" $ loginHandler authKey dbConn
  post "/signin" $ signinHandler authKey dbConn
  post "/graphql" $ Utils.graphqlHandler authKey dbConn interpretUserQuery
  post "/auth" $ isAuthorizedHandler authKey dbConn
  notFound $ Utils.makeErrorResponse 404 $ Just "Method not found"
