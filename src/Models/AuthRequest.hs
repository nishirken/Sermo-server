{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Models.AuthRequest where

import qualified Data.Yaml as Yaml
import Data.Yaml ((.=), (.:))
import Data.Text as Text

newtype AuthRequest = AuthRequest { _token :: Text.Text } deriving (Eq, Show)

instance Yaml.ToJSON AuthRequest where
  toJSON (AuthRequest token) = Yaml.object ["token" .= token]

instance Yaml.FromJSON AuthRequest where
  parseJSON (Yaml.Object v) = AuthRequest <$> v .: "token"
