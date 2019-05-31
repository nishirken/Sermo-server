{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Models.GraphQLRequest where

import qualified Data.Yaml as Yaml
import Data.Yaml ((.=), (.:))
import qualified Data.Text as T

data GraphQLRequest = GraphQLRequest
  { _token :: T.Text
  , _body :: T.Text
  }
  deriving (Eq, Show)

instance Yaml.ToJSON GraphQLRequest where
  toJSON GraphQLRequest { _token, _body } = Yaml.object
    [ "token" .= Yaml.toJSON _token
    , "body" .= Yaml.toJSON _body
    ]

instance Yaml.FromJSON GraphQLRequest where
  parseJSON (Yaml.Object v) = GraphQLRequest
    <$> v .: "token"
    <*> v .: "body"
