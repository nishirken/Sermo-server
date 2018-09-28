{-# LANGUAGE OverloadedStrings #-}

module Views.LoginForm where

import Lucid
import Views.BaseHtml

loginFormView :: Monad m => HtmlT m ()
loginFormView = baseHtml $
    div_ [class_ "form-page"] $ do
        (p_ "Login")
        form_ $ do
            input_ [type_ "text"]
            input_ [type_ "text"]
            input_ "Login" [type_ "submit"]
            
