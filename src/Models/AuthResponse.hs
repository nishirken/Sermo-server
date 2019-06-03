{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Models.AuthResponse where

import qualified Data.Yaml as Yaml
import Data.Yaml ((.=), (.:))
import Data.Text as Text

data AuthResponse = AuthResponse { _id :: Int, _token :: Text.Text } deriving (Eq, Show)

instance Yaml.ToJSON AuthResponse where
  toJSON AuthResponse { _id, _token } = Yaml.object
    [ "id" .= _id
    , "token" .= _token
    ]

instance Yaml.FromJSON AuthResponse where
  parseJSON (Yaml.Object v) = AuthResponse <$> v .: "id" <*> v .: "token"
