module Happaste.HTTP where

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
import Text.Blaze.Renderer.Text         (renderHtml)
import Text.Highlighter                 (lexerFromFilename, runLexer)
import Text.Highlighter.Formatters.Html (format)
import Text.Reform.Happstack            (happstackEitherForm)
import Text.Reform.HSP.Text             (form)
import Web.Routes                       (Site)
import Web.Routes.Boomerang             (Router, boomerangSiteRouteT, lit, anyString, integer, (</>), (<>))
import Web.Routes.Happstack             (seeOtherURL)

import Happaste.Forms
import Happaste.HTML
import Happaste.State
import Happaste.Types

assets :: Map FilePath ByteString
assets = Map.fromList $(embedDir "assets")

neverExpires :: Server ()
neverExpires = setHeaderM "Expires" "Mon, 31 Dec 2035 12:00:00 GMT"

sitemap :: Router Sitemap
sitemap = rAssetURL . lit "assets" </> anyString
       <> rCreatePasteURL
       <> rGetPasteURL . integer

site :: Site Sitemap (ServerPartT (ReaderT States (StateT Integer IO)) Response)
site = boomerangSiteRouteT route sitemap

route :: Sitemap -> Server Response

route (AssetURL f) = do
    neverExpires
    guessContentTypeM mimeTypes f >>= setHeaderM "Content-Type"
    maybe mzero (ok . toResponse) $ Map.lookup f assets

route (CreatePasteURL) = do
    r <- happstackEitherForm (form CreatePasteURL) "paste" pasteForm
    case r of
      Left f      -> createPastePage f
      Right paste -> update (CreatePaste paste) >>= seeOtherURL . GetPasteURL

route (GetPasteURL k) = do
    neverExpires
    queryMaybe (GetPaste k) $ \p -> do
      h <- get (T.unpack $ p ^. fileName) $ p ^. content
      getPastePage p h
  where
    get f t =
        query (GetHighlight k) >>= maybe create return
      where
        create   = update . SaveHighlight k . maybe t render $
                     lexerFromFilename f
        render l = either (const t) (L.toStrict . renderHtml . format False) $
                     runLexer l $ encodeUtf8 t
