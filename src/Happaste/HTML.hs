{-# OPTIONS_GHC -F -pgmF trhsx #-}

module Happaste.HTML where

import Prelude hiding (head)

import Control.Monad             (liftM)
import Data.Lens                 ((^.))
import Data.Text                 (Text, pack, unpack)
import Happstack.Server          (Response, ToMessage, getHeaderM, toResponse)
import Happstack.Server.HSP.HTML (EmbedAsChild(asChild), EmbedAsAttr, genElement, asAttr, Attr((:=)), unXMLGenT, genEElement, cdata)
import Happstack.Server.YUI      (gridUnit)
import Web.Routes.XMLGenT        ()

import Happaste.CSS   (css)
import Happaste.JS    (pjax)
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
          <script src="/yui/3.5.1"/>
          <% pjax %>
        </body>
      </html>

stylesheet :: EmbedAsAttr Server (Attr String url) => url -> Template
stylesheet url =
    <link rel="stylesheet" type="text/css" href=url/>

head :: Template
head =
    <head>
      <title>Happaste</title>
      <% stylesheet "/yui/3.5.1/css?reset&base&fonts&grids" %>
      <% stylesheet $ AssetURL "highlighter.css" %>
      <% stylesheet "http://fonts.googleapis.com/css?family=Stoke" %>
      <% css %>
    </head>

header :: Template
header =
    <div id="header">
      <% grid $ unit 1 1
        <h1><a href=CreatePasteURL class="pjax">Happaste</a></h1>
      %>
    </div>

grid :: EmbedAsChild Server c => c -> Template
grid body =
    <div class="grid">
      <div class="yui3-g">
        <% body %>
      </div>
    </div>

unit :: EmbedAsChild Server c => Integer -> Integer -> c -> Template
unit n d body =
    <div class=(gridUnit' n d)>
      <div class="unit">
        <% body %>
      </div>
    </div>
  where
    gridUnit' 1 1 = pack "yui3-u-1"
    gridUnit' a b = gridUnit a b

recentPastesList :: Template
recentPastesList = do
    ps <- query RecentPastes
    <ol class="recent-pastes">
      <% each ps $ \(k,p) ->
        <li><a href=(GetPasteURL k) class="pjax"><% p ^. fileName %></a></li>
      %>
    </ol>

createPastePage :: [Template] -> Server Response
createPastePage f =
    appTemplate
      <%>
        <% unit 17 24 f %>
        <% unit 7 24 recentPastesList %>
      </%>

getPastePage :: Paste -> Text -> Server Response
getPastePage p h =
    appTemplate $ unit 1 1
      <%>
        <h2><% p ^. fileName %></h2>
        <% cdata . unpack $ h %>
      </%>
