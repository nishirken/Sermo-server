module Views.BaseHtml where

import Lucid

baseHtml :: Monad m => HtmlT m () -> HtmlT m ()
baseHtml body =
    doctypehtml_ $
        body_ $ body
