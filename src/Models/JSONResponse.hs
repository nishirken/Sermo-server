{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Models.JSONResponse where

import qualified Data.Yaml as Yaml
import Data.Yaml ((.=), (.:))
import Models.JSONError (JSONError (..))

data JSONResponse a = JSONResponse
  { _data :: Maybe a
  , _error :: Maybe JSONError
  }
  deriving (Eq, Show)

instance Yaml.ToJSON a => Yaml.ToJSON (JSONResponse a) where
  toJSON JSONResponse { _data, _error } = Yaml.object
    [ "data" .= Yaml.toJSON _data
    , "error" .= Yaml.toJSON _error
    ]

instance Yaml.FromJSON a => Yaml.FromJSON (JSONResponse a) where
  parseJSON (Yaml.Object v) = JSONResponse
    <$> v .: "data"
    <*> v .: "error"
