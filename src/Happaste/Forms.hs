{-# OPTIONS_GHC -F -pgmF trhsx #-}

module Happaste.Forms where

import Control.Applicative       ((<$>), (<*>))
import Data.Text                 (pack)
import Happstack.Server.HSP.HTML (EmbedAsChild(asChild), genElement)
import Text.Digestive            ((++>), mapView)
import Text.Digestive.HSP.Html4  (label, inputText, inputTextArea)

import Happaste.Types

pasteForm :: Form e
pasteForm = mapView dl $ Paste
    <$>   dt `mapView` label "File name:"
      ++> dd `mapView` inputText Nothing
    <*> ( pack <$>
          dt `mapView` label "Paste:"
      ++> dd `mapView` inputTextArea (Just 80) (Just 24) Nothing
        )
  where
    dl x = [<dl><% x %></dl>]
    dt x = [<dt><% x %></dt>]
    dd x = [<dd><% x %></dd>]
