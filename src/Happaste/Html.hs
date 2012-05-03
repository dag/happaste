{-# OPTIONS_GHC -F -pgmF trhsx #-}

module Happaste.Html where

import Prelude hiding (head)

import Control.Monad             (liftM)
import Data.Lens                 ((^.))
import Data.Text                 (Text, unpack)
import Happstack.Server          (Response, ToMessage, getHeaderM, toResponse)
import Happstack.Server.HSP.HTML (EmbedAsChild(asChild), EmbedAsAttr, genElement, asAttr, Attr((:=)), unXMLGenT, genEElement, cdata)
import Text.Digestive.HSP.Html4  (form)
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
          <script src=(AssetURL "yui.js")/>
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
      <% stylesheet $ AssetURL "yui.css" %>
      <% stylesheet $ AssetURL "highlighter.css" %>
      <% stylesheet "http://fonts.googleapis.com/css?family=Stoke" %>
      <% css %>
    </head>

header :: Template
header =
    <div id="header">
      <% grid $ unit "1"
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

unit :: EmbedAsChild Server c => String -> c -> Template
unit size body =
    <div class=("yui3-u-" ++ size)>
      <div class="unit">
        <% body %>
      </div>
    </div>

createPasteForm :: [Template] -> Template
createPasteForm f =
    form CreatePasteURL
      <%>
        <% f %>
        <input type="submit" value="Create"/>
      </%>

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
        <% unit "17-24" $ createPasteForm f %>
        <% unit "7-24" recentPastesList %>
      </%>

getPastePage :: Paste -> Text -> Server Response
getPastePage p h =
    appTemplate $ unit "1"
      <%>
        <h2><% p ^. fileName %></h2>
        <% cdata . unpack $ h %>
      </%>
