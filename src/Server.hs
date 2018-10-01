{-# LANGUAGE OverloadedStrings #-}

module Server
    ( startServer
    ) where

import Web.Scotty (scotty)
import Routes (routes)
import Database.PostgreSQL.Simple (Connection, connectPostgreSQL, execute)

usersTableName = "users"

prepareDb :: Connection -> IO Connection
prepareDb conn = (execute conn "create table if not exists ? (email varchar(50), password varchar(200))", (,) usersTableName) >> return conn

port = 8080

startServer :: IO ()
startServer =
    (putStrLn $ "Server started at " ++ (show port)) >>
    (connectPostgreSQL "host='psql' port=5432 user='postgres' dbname='auth'") >>=
        \dbConn -> prepareDb >> scotty port (routes dbConn)
