{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Models.GraphQLErrorResponse where

import qualified Data.Yaml as Yaml
import Data.Yaml ((.=), (.:))
import qualified Data.Text as T

newtype GraphQLError = GraphQLError { _message :: T.Text } deriving (Eq, Show)

newtype GraphQLErrorResponse = GraphQLErrorResponse { _errors :: [GraphQLError] } deriving (Eq, Show)

instance Yaml.ToJSON GraphQLError where
  toJSON GraphQLError { _message } = Yaml.object
    [ "message" .= Yaml.toJSON _message
    ]

instance Yaml.FromJSON GraphQLError where
  parseJSON (Yaml.Object v) = GraphQLError
    <$> v .: "message"

instance Yaml.ToJSON GraphQLErrorResponse where
  toJSON GraphQLErrorResponse { _errors } = Yaml.object
    [ "errors" .= Yaml.toJSON _errors
    ]

instance Yaml.FromJSON GraphQLErrorResponse where
  parseJSON (Yaml.Object v) = GraphQLErrorResponse
    <$> v .: "errors"
