{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Models.LoginRequest where

import qualified Data.Yaml as Yaml
import Data.Yaml ((.=), (.:))
import qualified Data.Text as T

data LoginRequest = LoginRequest
  { _email :: T.Text
  , _password :: T.Text
  }
  deriving (Eq, Show)

instance Yaml.ToJSON LoginRequest where
  toJSON LoginRequest { _email, _password } = Yaml.object
    [ "email" .= Yaml.toJSON _email
    , "password" .= Yaml.toJSON _password
    ]

instance Yaml.FromJSON LoginRequest where
  parseJSON (Yaml.Object v) = LoginRequest
    <$> v .: "email"
    <*> v .: "password"
