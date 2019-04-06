{-# LANGUAGE OverloadedStrings #-}

module RestHandlers.Types where

import qualified Web.Scotty as Scotty
import qualified Network.HTTP.Types as NetworkTypes
import qualified Data.Yaml as Yaml
import qualified Data.Text as T
import Data.Yaml ((.=))

data JSONError = JSONError { code :: Int, message :: Maybe T.Text }

data JSONResponse a = JSONResponse
  { _data :: Maybe a
  , _error :: Maybe JSONError
  }

newtype SuccessResponse = SuccessResponse { success :: Bool }

data TokenRequest = TokenRequest
  { token :: T.Text
  , body :: T.Text
  }

instance Yaml.FromJSON TokenRequest where
  parseJSON (Yaml.Object v) = TokenRequest
      <$> v Yaml..: "token"
      <*> v Yaml..: "body"
