{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Models.GraphQLRequest where

import qualified Data.Yaml as Yaml
import Data.Yaml ((.=), (.:))
import qualified Data.Text as T

data GraphQLQuery = GraphQLQuery { _query :: T.Text } deriving (Eq, Show)

instance Yaml.ToJSON GraphQLQuery where
  toJSON GraphQLQuery { _query } = Yaml.object
    [ "query" .= Yaml.toJSON _query ]

instance Yaml.FromJSON GraphQLQuery where
  parseJSON (Yaml.Object v) = GraphQLQuery <$> v .: "query"

data GraphQLRequest = GraphQLRequest
  { _token :: T.Text
  , _body :: GraphQLQuery
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
