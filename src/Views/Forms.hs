{-# LANGUAGE OverloadedStrings #-}

module Views.Forms where

import Lucid

loginFormView :: Monad m => HtmlT m ()
loginFormView = form_ $ do
    input_ [type_ "mail"]
    input_ [type_ "password"]
    button_ [type_ "submit"] "Log in"

signinFormView :: Monad m => HtmlT m ()
signinFormView = form_ $ do
    input_ [type_ "mail"]
    input_ [type_ "password"]
    input_ [type_ "password"]
    button_ [type_ "submit"] "Sign in"
