{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Models.TokenObject where

import qualified Data.Yaml as Yaml
import Data.Yaml ((.=), (.:))
import Data.Text as Text

newtype TokenObject = TokenObject { _token :: Text.Text } deriving (Eq, Show)

instance Yaml.ToJSON TokenObject where
  toJSON (TokenObject token) = Yaml.object ["token" .= token]

instance Yaml.FromJSON TokenObject where
  parseJSON (Yaml.Object v) = TokenObject <$> v .: "token"
