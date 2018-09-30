{-# LANGUAGE OverloadedStrings #-}

module Views.SigninPage where

import Lucid
import Views.BaseHtml
import Views.SwitchButtons
import Views.ErrorTitle
import Models

signinFormView :: Monad m => HtmlT m ()
signinFormView = form_ [action_ "/signin", method_ "post"] $ do
    input_ [type_ "email" , name_ "email", required_ "true", placeholder_ "email"]
    input_ [type_ "password", name_ "password", required_ "true", placeholder_ "password"]
    input_ [type_ "password", name_ "repeatedPassword", required_ "true", placeholder_ "repeat password"]
    button_ [type_ "submit"] "Sign in"

signinPageView :: Monad m => FormPageView -> HtmlT m ()
signinPageView (FormPageView error) = baseHtml $ do
    switchButtonsView False
    signinFormView
    errorTitle error

