{-# OPTIONS_GHC -F -pgmF trhsx #-}

module Happaste.Forms where

import qualified HSX.XMLGenerator as HSX

import Control.Applicative       ((<$>), (<*>))
import Data.Text                 (pack)
import Happstack.Server          (Input)
import Happstack.Server.HSP.HTML (EmbedAsChild(asChild), genElement, XMLGenT)
import Text.Digestive            ((++>), Form, mapView)
import Text.Digestive.HSP.Html4  (label, inputText, inputTextArea)

import Happaste.Types

pasteForm :: Form Server [Input] e [XMLGenT Server (HSX.XML Server)] Paste
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
