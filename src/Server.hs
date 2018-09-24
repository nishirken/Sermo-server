module Server
    ( startServer
    ) where

import Web.Scotty (scotty)
import Routes (routes)

port = 8080

startServer :: IO ()
startServer =
    (putStrLn $ "Server started at " ++ (show port)) >>
    scotty port routes
