{-# LANGUAGE OverloadedStrings #-}

module Views.SwitchButtons where

import Lucid

linkClass x = if x then "enabled" else "disabled"

switchButtonsView :: Monad m => Bool -> HtmlT m ()
switchButtonsView isActive = do
    a_ [href_ "/login", class_ $ linkClass isActive] "Log in"
    a_ [href_ "/SignIn", class_ $ linkClass $ not isActive] "Sign in"
