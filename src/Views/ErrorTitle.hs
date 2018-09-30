{-# LANGUAGE OverloadedStrings #-}

module Views.ErrorTitle where

import Lucid
import Data.Text.Lazy (Text)

errorTitle :: Monad m => Text -> HtmlT m ()
errorTitle errorMessage = h6_ [class_ "error"] $ toHtml errorMessage
