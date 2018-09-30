{-# LANGUAGE OverloadedStrings #-}

module Server
    ( startServer
    ) where

import Web.Scotty (scotty)
import Routes (routes)
import Database.PostgreSQL.Simple (connectPostgreSQL)

port = 8080

startServer :: IO ()
startServer =
    (putStrLn $ "Server started at " ++ (show port)) >>
    (connectPostgreSQL "host='psql' port=5432 user='postgres'") >>= \dbConn ->
        scotty port (routes dbConn)
