module Main where

import Prelude hiding (id, (.))

import qualified Data.Map       as Map
import qualified Data.Text      as T
import qualified Data.Text.Lazy as L

import Control.Applicative              ((<$>), (<*>))
import Control.Category                 (Category(id, (.)))
import Control.Exception                (bracket)
import Control.Monad                    (forM, mzero)
import Control.Monad.Reader             (ask, asks, ReaderT, runReaderT)
import Control.Monad.Trans              (lift, liftIO)
import Data.Acid                        (AcidState, Query, Update, makeAcidic, openLocalState)
import Data.Acid.Advanced               (query', update')
import Data.Acid.Local                  (createCheckpointAndClose)
import Data.ByteString                  (ByteString)
import Data.Default                     (Default(def))
import Data.FileEmbed                   (embedDir)
import Data.IxSet                       (IxSet, Indexable(empty), ixSet, ixFun, insert, getOne, (@=), toDescList, Proxy(Proxy))
import Data.Lens                        ((^.), (+=), (%=), getL)
import Data.Lens.Template               (makeLens)
import Data.Map                         (Map, (!))
import Data.SafeCopy                    (base, deriveSafeCopy)
import Data.Text                        (Text, pack)
import Data.Text.Encoding               (encodeUtf8)
import Data.Typeable                    (Typeable)
import Data.Unique                      (hashUnique, newUnique)
import HSP.ServerPartT                  ()
import HSX.JMacro                       (IntegerSupply(nextInteger))
import Happstack.Server                 (ServerPartT, mapServerPartT, Response, toResponse, ok, setHeaderM, simpleHTTP, nullConf, decodeBody, defaultBodyPolicy)
import Happstack.Server.FileServe       (guessContentTypeM, mimeTypes)
import Happstack.Server.HSP.HTML        (EmbedAsChild(asChild), cdata, genElement, asAttr, Attr((:=)), unXMLGenT, genEElement)
import Happstack.Server.JMacro          ()
import Language.Javascript.JMacro
import Text.Blaze.Renderer.Text         (renderHtml)
import Text.Boomerang.TH                (derivePrinterParsers)
import Text.Digestive                   ((++>))
import Text.Digestive.Forms.Happstack   (eitherHappstackForm)
import Text.Digestive.HSP.Html4         (label, inputText, inputTextArea, form)
import Text.Highlighter                 (lexerFromFilename, runLexer)
import Text.Highlighter.Formatters.Html (format)
import Text.Lucius                      (Css, renderCss, lucius)
import Web.Routes                       (Site, RouteT, askRouteT)
import Web.Routes.Boomerang             (Router, boomerangSiteRouteT, lit, anyString, integer, (</>), (<>))
import Web.Routes.Happstack             (implSite, seeOtherURL)
import Web.Routes.XMLGenT               ()


{---------------------------------------------------------------------------
 -                                  State                                  -
 ---------------------------------------------------------------------------}

type Key = Integer

data Paste = Paste
  { _fileName :: Text
  , _content  :: Text
  } deriving (Typeable, Ord, Eq)
makeLens ''Paste
deriveSafeCopy 0 'base ''Paste

instance Indexable (Key,Paste) where
  empty = ixSet
      [ ixFun $ \(k,_) -> [k]
      , ixFun $ \(_,p) -> [getExt $ p ^. fileName]
      ]
    where
      getExt = T.dropWhile (== '.') . T.dropWhile (/= '.')

instance Indexable a => Default (IxSet a) where
  def = empty

data AppState = AppState
  { _nextKey :: Key
  , _pastes  :: IxSet (Key,Paste)
  } deriving Typeable
makeLens ''AppState
deriveSafeCopy 0 'base ''AppState

instance Default AppState where
  def = AppState def def

type State = AcidState AppState

recentPastes :: Query AppState [(Key,Paste)]
recentPastes = do
    ps <- asks $ getL pastes
    return $ take 10 $ toDescList (Proxy :: Proxy Key) ps

savePaste :: Paste -> Update AppState Key
savePaste p = do
    k <- nextKey += 1
    pastes %= insert (k,p)
    return k

getPaste :: Key -> Query AppState (Maybe Paste)
getPaste k = do
    ps <- asks $ getL pastes
    return $ snd <$> (getOne $ ps @= k)

makeAcidic ''AppState ['recentPastes, 'savePaste, 'getPaste]

query ev = do
    st <- ask
    query' st ev

update ev = do
    st <- ask
    update' st ev

queryMaybe ev f = do
    m <- query ev
    maybe mzero f m


{---------------------------------------------------------------------------
 -                                 Routing                                 -
 ---------------------------------------------------------------------------}

data Sitemap = Asset FilePath | NewPaste | ShowPaste Key
derivePrinterParsers ''Sitemap

sitemap :: Router Sitemap
sitemap = (rAsset . (lit "assets" </> anyString))
       <> (rNewPaste)
       <> (rShowPaste . integer)

site :: Site Sitemap (ServerPartT (ReaderT State IO) Response)
site = boomerangSiteRouteT route sitemap

route :: Sitemap -> RouteT Sitemap (ServerPartT (ReaderT State IO)) Response

route (Asset f) = do
    mime <- guessContentTypeM mimeTypes f
    setHeaderM "Content-Type" mime
    ok $ toResponse $ assets ! f

route (NewPaste) = do
    r <- eitherHappstackForm pasteForm "paste"
    case r of
      Left f -> appTemplate
        <%>
          <div class="yui3-u-17-24">
            <% form NewPaste
              <%>
                <% f %>
                <input type="submit" value="Create"/>
              </%>
            %>
          </div>
          <div class="yui3-u-7-24 recent-pastes">
            <% recentPastesList %>
          </div>
        </%>
      Right paste -> do
        k <- update $ SavePaste paste
        seeOtherURL $ ShowPaste k

route (ShowPaste k) = do
    queryMaybe (GetPaste k) $ \paste -> do
      let text        = paste ^. content
          highlighted =
            case lexerFromFilename . T.unpack $ paste ^. fileName of
              Nothing    -> text
              Just lexer ->
                case runLexer lexer $ encodeUtf8 $ text of
                  Left _       -> text
                  Right tokens ->
                    L.toStrict . renderHtml $ format False tokens
      appTemplate
        <%>
          <div class="yui3-u-1">
            <h2><% paste ^. fileName %></h2>
            <pre><% cdata . T.unpack $ highlighted %></pre>
          </div>
        </%>


{---------------------------------------------------------------------------
 -                                Templates                                -
 ---------------------------------------------------------------------------}

type Lucius url = (url -> [(Text,Maybe Text)] -> Text) -> Css

instance (Functor m, Monad m) => EmbedAsChild (RouteT url m) (Lucius url) where
  asChild style = do
    url <- lift askRouteT
    asChild
      <style type="text/css">
        <% renderCss $ style url %>
      </style>

instance IntegerSupply (ServerPartT IO) where
  nextInteger = fromIntegral . (`mod` 1024) . hashUnique <$> liftIO newUnique

appTemplate body = fmap toResponse $ unXMLGenT
    <html>
      <head>
        <link rel="stylesheet" type="text/css" href=(Asset "yui.css")/>
        <link rel="stylesheet" type="text/css" href=(Asset "highlighter.css")/>
        <link rel="stylesheet" type="text/css"
          href="http://fonts.googleapis.com/css?family=Gloria+Hallelujah&text=Happaste"/>
        <% css %>
      </head>
      <body>
        <div id="header">
          <h1><a href=NewPaste>Happaste</a></h1>
        </div>
        <div id="content" class="yui3-g">
          <% body %>
        </div>
      </body>
    </html>

recentPastesList = do
    ps <- query RecentPastes
    asChild
      <ol>
        <% forM ps $ \(k,p) ->
          <li><a href=(ShowPaste k)><% p ^. fileName %></a></li>
        %>
      </ol>

css :: Lucius Sitemap
css = [$lucius|
  @width: 960px;

  div#header
    { background : #3B4162
    ; h1
        { font-family : "Gloria Hallelujah", serif
        ; font-size   : 197%
        ; margin      : 0 auto
        ; padding     : .5em 0
        ; max-width   : #{width}
        ; a
            { color           : #fff
            ; text-decoration : none
            }
        }
    }

  div#content
    { margin    : 1em auto
    ; max-width : #{width}
    ; font-size : 123.1%
    ; label, textarea
        { display : block
        }
      textarea
        { font-family : monospace
        ; width       : 100%
        ; height      : 2400%
        }
      a
        { color           : #3B4162
        ; text-decoration : none
        }
      .recent-pastes ol
        { list-style-type : none
        ; margin-left     : 40px
        ; padding-left    : 20px
        ; border-left     : 3px solid #3B4162
        }
    }
|]

assets :: Map FilePath ByteString
assets = Map.fromList $(embedDir "assets")


{---------------------------------------------------------------------------
 -                                  Forms                                  -
 ---------------------------------------------------------------------------}

pasteForm = Paste
    <$>           label "File name:"
              ++> inputText Nothing
    <*> (pack <$> label "Paste:"
              ++> inputTextArea (Just 80) (Just 24) Nothing)


{---------------------------------------------------------------------------
 -                               Application                               -
 ---------------------------------------------------------------------------}

server :: State -> IO ()
server st = simpleHTTP nullConf $ do
    decodeBody $ defaultBodyPolicy "/tmp/" 0 40960 40960
    implSite (pack "http://localhost:8000") T.empty $
      fmap (mapServerPartT (`runReaderT` st)) site

main :: IO ()
main = bracket (openLocalState def) createCheckpointAndClose server
