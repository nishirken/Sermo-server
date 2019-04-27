{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Models.JSONError where

import qualified Data.Yaml as Yaml
import Data.Yaml ((.=), (.:))
import qualified Data.Text as T

data JSONError = JSONError { _code :: Int, _message :: Maybe T.Text } deriving (Eq, Show)

instance Yaml.ToJSON JSONError where
  toJSON JSONError { _code, _message } = Yaml.object
    [ "code" .= Yaml.toJSON _code
    , "message" .= Yaml.toJSON _message
    ]

instance Yaml.FromJSON JSONError where
  parseJSON (Yaml.Object v) = JSONError
    <$> v .: "code"
    <*> v .: "message"
