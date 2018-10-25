{-# LANGUAGE OverloadedStrings #-}

module Config where

import Data.Text.Lazy (Text, toStrict, pack)
import Data.Yaml (FromJSON, Value (Object), parseJSON, (.:))
import Data.Text.Encoding (encodeUtf8)

data Config = Config {
    dbHost :: Text
    , dbPort :: Int
    , dbUser :: Text
    , dbName :: Text
    , appPort :: Int
    , authKey :: Text
}

instance FromJSON Config where
    parseJSON (Object v) = Config
        <$> v .: "dbHost"
        <*> v .: "dbPort"
        <*> v .: "dbUser"
        <*> v .: "dbName"
        <*> v .: "appPort"
        <*> v .: "authKey"

    parseJSON invalid = error $ "Can't parse Config from Yaml" <> show invalid
