{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Models.SuccessResponse where

import qualified Data.Yaml as Yaml
import Data.Yaml ((.=), (.:))

newtype SuccessResponse = SuccessResponse { success :: Bool } deriving (Eq, Show)

instance Yaml.ToJSON SuccessResponse where
  toJSON SuccessResponse { success } = Yaml.object [ "success" .= Yaml.toJSON success ]

instance Yaml.FromJSON SuccessResponse where
  parseJSON (Yaml.Object v) = SuccessResponse <$> v .: "success"
