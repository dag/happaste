{-# OPTIONS_GHC -F -pgmF trhsx #-}

module Happaste.Html where

import Prelude hiding (head)

import qualified Data.Text        as T
import qualified HSX.XMLGenerator as HSX

import Control.Monad             (liftM)
import Data.Lens                 ((^.))
import Data.Text                 (Text)
import Happstack.Server          (Response, ToMessage, getHeaderM, toResponse)
import Happstack.Server.HSP.HTML (EmbedAsChild(asChild), EmbedAsAttr, genElement, asAttr, Attr((:=)), XMLGenT, unXMLGenT, genEElement, cdata)
import Text.Digestive.HSP.Html4  (form)
import Web.Routes.XMLGenT        ()

import Happaste.Css     (css)
import Happaste.Scripts (pjax)
import Happaste.State
import Happaste.Types

type Template = XMLGenT Server (HSX.XML Server)

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

stylesheet :: EmbedAsAttr Server (Attr String url) => url -> Template
stylesheet url =
    <link rel="stylesheet" type="text/css" href=url/>

head :: Template
head =
    <head>
      <title>Happaste</title>
      <% stylesheet $ Asset "yui.css" %>
      <% stylesheet $ Asset "highlighter.css" %>
      <% stylesheet "http://fonts.googleapis.com/css?family=Stoke" %>
      <% css %>
    </head>

header :: Template
header =
    <div id="header">
      <div class="grid">
        <div class="yui3-g">
          <% unit "1" <h1><a href=NewPaste class="pjax">Happaste</a></h1> %>
        </div>
      </div>
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

newPasteForm :: [Template] -> Template
newPasteForm f =
    form NewPaste
      <%>
        <% f %>
        <input type="submit" value="Create"/>
      </%>

recentPastesList :: Template
recentPastesList = query RecentPastes >>= \ps ->
    <ol class="recent-pastes">
      <% each ps $ \(k,p) ->
        <li><a href=(ShowPaste k) class="pjax"><% p ^. fileName %></a></li>
      %>
    </ol>

newPastePage :: [Template] -> Server Response
newPastePage f =
    appTemplate
      <%>
        <% unit "17-24" $ newPasteForm f %>
        <% unit "7-24" recentPastesList %>
      </%>

showPastePage :: Paste -> Text -> Server Response
showPastePage p h =
    appTemplate $ unit "1"
      <%>
        <h2><% p ^. fileName %></h2>
        <% cdata . T.unpack $ h %>
      </%>
