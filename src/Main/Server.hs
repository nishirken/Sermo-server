{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}

module Main.Server (testResponse) where

import Data.Text (Text)
import Data.Monoid ((<>))
import GraphQL (Response, interpretAnonymousQuery)
import GraphQL.API (Argument, Object, Field, (:>))
import GraphQL.Resolver (Handler)

type Hello = Object "Hello" '[]
  '[ Argument "who" Text :> Field "greeting" Text ]

hello :: Handler IO Hello
hello = pure (\who -> pure ("Hello " <> who))

testResponse :: Text -> IO Response
testResponse = interpretAnonymousQuery @Hello hello
