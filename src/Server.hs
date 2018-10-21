{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Server
    ( startServer
    ) where

import Web.Scotty (scotty)
import Control.Monad.IO.Class (liftIO)
import Routes (routes)
import Database.PostgreSQL.Simple (Connection, connectPostgreSQL, execute)
import Data.Yaml (decodeFileEither, ParseException)
import Config (Config (..))
import Data.Text.Lazy (toStrict, pack)
import Data.Text.Encoding (encodeUtf8)
import System.Directory (getCurrentDirectory)

prepareDb :: Connection -> IO Connection
prepareDb conn = do
    execute
        conn
        ("create table if not exists users " <>
        "(id serial primary key, email varchar(50) not null, password varchar(500) not null unique)")
        ()
    return conn

startServer :: IO ()
startServer = do
    directory <- getCurrentDirectory
    config <- decodeFileEither (directory <> "/config.yaml") :: IO (Either ParseException Config)
    case config of
        (Right conf@Config{ appPort }) -> do
            putStrLn $ "Server started at " <> show appPort
            dbConn <- connectPostgreSQL $ connectionString conf
            prepareDb dbConn >> scotty appPort (routes dbConn conf)
        (Left configError) -> print configError

        where
            connectionString Config{ dbHost, dbPort, dbUser, dbName } = encodeUtf8 . toStrict $
                "host='" <> dbHost <> "' "
                <> "port=" <> pack (show dbPort) <> " "
                <> "user='" <> dbUser <> "' "
                <> "dbname='" <> dbName <> "'"
