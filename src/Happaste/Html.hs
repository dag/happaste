{-# OPTIONS_GHC -F -pgmF trhsx #-}

module Happaste.Html where

import qualified HSX.XMLGenerator as HSX

import Control.Monad             (forM, liftM)
import Data.Lens                 ((^.))
import HSX.JMacro                (IntegerSupply)
import Happstack.Server          (Response, ToMessage, toResponse)
import Happstack.Server.HSP.HTML (EmbedAsChild(asChild), EmbedAsAttr, genElement, asAttr, Attr((:=)), XMLGenT, unXMLGenT, XMLGenerator, genEElement)
import Language.Css.Syntax       (StyleSheet)
import Web.Routes.XMLGenT        ()

import Happaste.Css (css)
import Happaste.State
import Happaste.Types

appTemplate ::
    ( EmbedAsChild m StyleSheet
    , EmbedAsChild m c
    , XMLGenerator m
    , ToMessage (HSX.XML m)
    , EmbedAsAttr m (Attr String Sitemap)
    , IntegerSupply m
    ) => c -> m Response
appTemplate body = liftM toResponse $ unXMLGenT
    <html>
      <head>
        <% stylesheet $ Asset "yui.css" %>
        <% stylesheet $ Asset "highlighter.css" %>
        <% stylesheet "http://fonts.googleapis.com/css?family=Stoke" %>
        <% css %>
      </head>
      <body>
        <div id="header">
          <div class="grid">
            <div class="yui3-g">
              <% unit "1"
                <h1><a href=NewPaste>Happaste</a></h1>
              %>
            </div>
          </div>
        </div>
        <div id="content">
          <div class="grid">
            <div class="yui3-g">
              <% body %>
            </div>
          </div>
        </div>
      </body>
    </html>
  where
    stylesheet :: (XMLGenerator x, EmbedAsAttr x (Attr String url))
               => url -> XMLGenT x (HSX.XML x)
    stylesheet url =
      <link rel="stylesheet" type="text/css" href=url/>

unit ::
    ( EmbedAsChild m c
    , EmbedAsChild m (HSX.XML m)
    , EmbedAsAttr m (Attr String String)
    ) => String -> c -> XMLGenT m (HSX.XML m)
unit size body =
    <div class=("yui3-u-" ++ size)>
      <div class="unit">
        <% body %>
      </div>
    </div>

recentPastesList :: XMLGenT Server (HSX.XML Server)
recentPastesList = do
    { ps <- query RecentPastes
    ; <ol class="recent-pastes">
        <% forM ps $ \(k,p) ->
          <li><a href=(ShowPaste k)><% p ^. fileName %></a></li>
        %>
      </ol>
    }
