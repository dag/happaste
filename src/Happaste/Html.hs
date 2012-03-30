{-# OPTIONS_GHC -F -pgmF trhsx #-}

module Happaste.Html where

import qualified HSX.XMLGenerator as HSX

import Control.Monad             (liftM)
import Data.Lens                 ((^.))
import Happstack.Server          (Response, ToMessage, getHeaderM, toResponse)
import Happstack.Server.HSP.HTML (EmbedAsChild(asChild), EmbedAsAttr, genElement, asAttr, Attr((:=)), XMLGenT, unXMLGenT, XMLGenerator, genEElement)
import Web.Routes.XMLGenT        ()
import Language.Javascript.JMacro

import Happaste.Css (css)
import Happaste.State
import Happaste.Types

each :: [a] -> (a -> b) -> [b]
each = flip map

appTemplate :: (EmbedAsChild Server c) => c -> Server Response
appTemplate body = do
    pjax <- getHeaderM "X-PJAX"
    liftM toResponse $ unXMLGenT $
      case pjax of
        Just _ ->
          <div class="grid">
            <div class="yui3-g">
              <% body %>
            </div>
          </div>
        Nothing ->
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
                      <h1><a href=NewPaste class="pjax">Happaste</a></h1>
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
              <script src=(Asset "yui.js")/>
              <% [$jmacro|
                YUI().use "pjax" \y ->
                  new y.Pjax { container: "#content"
                             , linkSelector: "a.pjax"
                             }
              |] %>
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
recentPastesList = query RecentPastes >>= \ps ->
    <ol class="recent-pastes">
      <% each ps $ \(k,p) ->
        <li><a href=(ShowPaste k) class="pjax"><% p ^. fileName %></a></li>
      %>
    </ol>
