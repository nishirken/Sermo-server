module Controllers.NotFound where

import Web.Scotty (html, ActionM)
import Lucid (renderText)
import Views.NotFound

notFoundController :: ActionM ()
notFoundController = html . renderText $ notFoundView
