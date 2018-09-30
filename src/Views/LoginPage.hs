{-# LANGUAGE OverloadedStrings #-}

module Views.LoginPage where

import Lucid
import Views.BaseHtml (baseHtml)
import Views.SwitchButtons (switchButtonsView)
import Views.ErrorTitle (errorTitle)
import Models

loginFormView :: Monad m => HtmlT m ()
loginFormView = form_ [action_ "/login", method_ "post"] $ do
    input_ [type_ "email", required_ "true", placeholder_ "email"]
    input_ [type_ "password", required_ "true", placeholder_ "password"]
    button_ [type_ "submit"] "Log in"

loginPageView :: Monad m => FormPageView -> HtmlT m ()
loginPageView (FormPageView error) = baseHtml $ do
    switchButtonsView True
    loginFormView
    errorTitle error
