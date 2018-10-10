{-# LANGUAGE OverloadedStrings #-}

module Server
    ( startServer
    ) where

import Web.Scotty (scotty)
import Control.Monad.IO.Class (liftIO)
import Routes (routes)
import Database.PostgreSQL.Simple (Connection, connectPostgreSQL, execute)

prepareDb :: Connection -> IO Connection
prepareDb conn = do
    execute
        conn
        ("create table if not exists users " <>
        "(id serial primary key, email varchar(50) not null, password varchar(500) not null unique)")
        ()
    return conn

port = 8080

startServer :: IO ()
startServer =
    (putStrLn $ "Server started at " ++ (show port)) >>
    (connectPostgreSQL "host='psql' port=5432 user='postgres' dbname='auth'") >>=
        \dbConn -> liftIO $ prepareDb dbConn >> scotty port (routes dbConn)
