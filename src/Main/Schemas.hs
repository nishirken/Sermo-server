{-# LANGUAGE OverloadedStrings #-}
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

type Hello = Object "Hello" '[]
  '[ Argument "who" T.Text :> Field "greeting" T.Text ]

hello :: Handler IO Hello
hello = pure (\who -> pure ("Hello " <> who))

graphqlResponse :: T.Text -> IO Response
graphqlResponse = interpretAnonymousQuery @Hello hello

graphqlHandler :: T.Text -> ActionM ()
graphqlHandler authKey = do
  TokenRequest { body, token } <- jsonData :: ActionM TokenRequest
  case verifiedToken authKey token of
    (Just _) -> (liftIO $ graphqlResponse body) >>= json
    Nothing -> status status401
