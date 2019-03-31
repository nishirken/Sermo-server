{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main.Schemas (graphqlHandler) where

import qualified Data.Text as T
import Data.Monoid ((<>))
import GraphQL (Response, interpretAnonymousQuery)
import GraphQL.API (Argument, Object, Field, (:>))
import GraphQL.Resolver (Handler)
import RestHandlers.Types (TokenRequest (..))
import Network.HTTP.Types (status401)
import Web.Scotty (ActionM, status, jsonData, json)
import RestHandlers.Auth (verifiedToken)
import Control.Monad.IO.Class (liftIO)
import Database.PostgreSQL.Simple (Connection, Only (..))
import qualified Db

type User = Object "User" '[]
  '[ Argument "id" T.Text :> Field "user" T.Text ]

userHandler :: Connection -> Handler IO User
userHandler dbConn =
  pure $ \userId -> do
    userResult <- Db.getUserById dbConn (read @Int (T.unpack userId))
    pure $ (fst . head) userResult

queryHandler :: Connection -> T.Text -> IO Response
queryHandler dbConn = interpretAnonymousQuery @User $ userHandler dbConn

graphqlHandler :: Connection -> T.Text -> ActionM ()
graphqlHandler dbConn authKey = do
  TokenRequest { body, token } <- jsonData :: ActionM TokenRequest
  case verifiedToken authKey token of
    (Just _) -> liftIO (queryHandler dbConn body) >>= json
    Nothing -> status status401
