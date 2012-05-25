{-# OPTIONS_GHC -F -pgmF trhsx #-}

module Happaste.Forms where

import qualified Data.Text as T

import Control.Applicative       ((<$>), (<*>), (<*))
import Happstack.Server.HSP.HTML (EmbedAsChild(asChild), genElement)
import Text.Reform               ((++>), mapView)
import Text.Reform.Happstack     ()
import Text.Reform.HSP.Text      (label, inputText, textarea, buttonSubmit)

import Happaste.Types

pasteForm :: Form Paste
pasteForm = mapView dl $
    Paste <$> dt `mapView` label "File name:" ++> dd `mapView` inputText T.empty
          <*> dt `mapView` label "Paste:"     ++> dd `mapView` textarea 80 24 T.empty
          <*  buttonSubmit T.empty "Create"
  where
    dl x = [<dl><% x %></dl>]
    dt x = [<dt><% x %></dt>]
    dd x = [<dd><% x %></dd>]
