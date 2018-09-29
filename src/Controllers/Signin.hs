module Controllers.Signin where

import Web.Scotty (html, ActionM)
import Lucid (renderText)
import Views.LoginPage

signinController :: ActionM ()
signinController =
    undefined
