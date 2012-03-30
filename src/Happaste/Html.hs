{-# OPTIONS_GHC -F -pgmF trhsx #-}

module Happaste.Html where

import Prelude hiding (head)

import qualified HSX.XMLGenerator as HSX

import Control.Monad             (liftM)
import Data.Lens                 ((^.))
import Happstack.Server          (Response, ToMessage, getHeaderM, toResponse)
import Happstack.Server.HSP.HTML (EmbedAsChild(asChild), EmbedAsAttr, genElement, asAttr, Attr((:=)), XMLGenT, unXMLGenT, XMLGenerator, genEElement)
import Web.Routes.XMLGenT        ()

import Happaste.Css     (css)
import Happaste.Scripts (pjax)
import Happaste.State
import Happaste.Types

each :: [a] -> (a -> b) -> [b]
each = flip map

appTemplate :: EmbedAsChild Server c => c -> Server Response
appTemplate body = do
    xpjax <- getHeaderM "X-PJAX"
    liftM toResponse $ unXMLGenT $ maybe html (const $ grid body) xpjax
  where
    html =
      <html>
        <% head %>
        <body>
          <% header %>
          <div id="content"><% grid body %></div>
          <script src=(Asset "yui.js")/>
          <% pjax %>
        </body>
      </html>

stylesheet ::
    ( XMLGenerator m
    , EmbedAsAttr m (Attr String url)
    ) => url -> XMLGenT m (HSX.XML m)
stylesheet url =
  <link rel="stylesheet" type="text/css" href=url/>

head :: XMLGenT Server (HSX.XML Server)
head =
  <head>
    <% stylesheet $ Asset "yui.css" %>
    <% stylesheet $ Asset "highlighter.css" %>
    <% stylesheet "http://fonts.googleapis.com/css?family=Stoke" %>
    <% css %>
  </head>

header :: XMLGenT Server (HSX.XML Server)
header =
  <div id="header">
    <div class="grid">
      <div class="yui3-g">
        <% unit "1" <h1><a href=NewPaste class="pjax">Happaste</a></h1> %>
      </div>
    </div>
  </div>

grid :: EmbedAsChild Server c => c -> XMLGenT Server (HSX.XML Server)
grid body =
  <div class="grid">
    <div class="yui3-g">
      <% body %>
    </div>
  </div>

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
