{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Routes (routes) where

import Web.Scotty (get, post, html, ScottyM, notFound, middleware, status, json, jsonData, ActionM)
import Network.Wai.Middleware.Static (static)
import Network.Wai.Middleware.Cors (cors, CorsResourcePolicy (..))
import Database.PostgreSQL.Simple (Connection)
import Handlers.LogIn
import Handlers.SignIn
import Handlers.Crypt (authMiddleware)
import Handlers.Utils (makeStatus)
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text as T
import Data.CaseInsensitive (mk)
import Config (Config (..))
import Main.Server (testResponse)
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

data JSONRequest = JSONRequest {
  token :: T.Text
  , body :: T.Text
}

routes :: Connection -> Config -> IO (ScottyM ())
routes dbConn Config{ authKey } = do
  JSONRequest { body } <- jsonData :: ActionM JSONRequest
  graphqlResponse <- testResponse body
  pure $ do
    middleware static
    middleware $ cors $ \_ -> Just corsConfig
    middleware $ authMiddleware authKey
    post "/login" $ logInHandler authKey dbConn
    post "/signin" $ signInHandler authKey dbConn
    post "/graphql" $ json graphqlResponse
    notFound $ makeStatus 404 "Method not found"
