{-# OPTIONS_GHC -F -pgmF trhsx #-}

module Happaste.Routes where

import Prelude hiding ((.))

import qualified Data.Map         as Map
import qualified Data.Text        as T
import qualified Data.Text.Lazy   as L

import Control.Category                 (Category((.)))
import Control.Monad                    (MonadPlus, mzero)
import Control.Monad.Reader             (ReaderT)
import Control.Monad.State              (StateT)
import Data.ByteString                  (ByteString)
import Data.FileEmbed                   (embedDir)
import Data.Lens                        ((^.))
import Data.Map                         (Map)
import Data.Text.Encoding               (encodeUtf8)
import Happstack.Server                 (ServerPartT, Response, ToMessage, toResponse, ok, setHeaderM)
import Happstack.Server.FileServe       (guessContentTypeM, mimeTypes)
import Happstack.Server.HSP.HTML        (EmbedAsChild(asChild), EmbedAsAttr, cdata, genElement, asAttr, Attr((:=)), genEElement)
import Text.Blaze.Renderer.Text         (renderHtml)
import Text.Digestive.Forms.Happstack   (eitherHappstackForm)
import Text.Digestive.HSP.Html4         (form)
import Text.Highlighter                 (lexerFromFilename, runLexer)
import Text.Highlighter.Formatters.Html (format)
import Web.Routes                       (Site)
import Web.Routes.Boomerang             (Router, boomerangSiteRouteT, lit, anyString, integer, (</>), (<>))
import Web.Routes.Happstack             (seeOtherURL)

import Happaste.Forms
import Happaste.Html
import Happaste.State
import Happaste.Types

assets :: Map FilePath ByteString
assets = Map.fromList $(embedDir "assets")

sitemap :: Router Sitemap
sitemap = (rAsset . (lit "assets" </> anyString))
       <> (rNewPaste)
       <> (rShowPaste . integer)

site :: Site Sitemap (ServerPartT (ReaderT States (StateT Integer IO)) Response)
site = boomerangSiteRouteT route sitemap

route :: Sitemap -> Server Response

route (Asset f) = do
    mime <- guessContentTypeM mimeTypes f
    setHeaderM "Content-Type" mime
    maybe mzero (ok . toResponse) $ Map.lookup f assets

route (NewPaste) = do
    r <- eitherHappstackForm pasteForm "paste"
    case r of
      Left f -> appTemplate
        <%>
          <% unit "17-24" $ form NewPaste
            <%>
              <% f %>
              <input type="submit" value="Create"/>
            </%>
          %>
          <% unit "7-24" recentPasteStateList %>
        </%>
      Right paste -> do
        k <- update $ SavePaste paste
        seeOtherURL $ ShowPaste k

route (ShowPaste k) =
    queryMaybe (GetPaste k) $ \p -> do
      highlighted <- get (T.unpack $ p ^. fileName) $ p ^. content
      appTemplate $ unit "1"
        <%>
          <h2><% p ^. fileName %></h2>
          <% cdata . T.unpack $ highlighted %>
        </%>
  where
    get f t =
        query (GetHighlight k) >>= maybe create return
      where
        create   = update . SaveHighlight k . maybe t render $
                     lexerFromFilename f
        render l = either (const t) (L.toStrict . renderHtml . format False) $
                     runLexer l $ encodeUtf8 t
