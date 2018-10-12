{-# LANGUAGE OverloadedStrings #-}

module Controllers.LogOut where

import Web.Scotty (addHeader, ActionM, redirect)

logOutController :: ActionM ()
logOutController = do
    addHeader "Set-Cookie" "token=deleted;"
    redirect "/login"
