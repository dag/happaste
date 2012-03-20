module Main where

import Prelude hiding (id, (.))

import qualified Data.Map         as Map
import qualified Data.Text        as T
import qualified Data.Text.Lazy   as L
import qualified HSX.XMLGenerator as HSX

import Control.Applicative              ((<$>), (<*>))
import Control.Category                 (Category(id, (.)))
import Control.Exception                (bracket)
import Control.Monad                    (MonadPlus, forM, mzero, liftM)
import Control.Monad.Reader             (MonadReader, ask, asks, ReaderT, runReaderT)
import Control.Monad.State              (StateT, evalStateT)
import Control.Monad.Trans              (MonadIO, lift)
import Data.Acid                        (AcidState, QueryEvent, UpdateEvent, Query, Update, EventResult, makeAcidic, openLocalState)
import Data.Acid.Advanced               (MethodState, MethodResult, query', update')
import Data.Acid.Local                  (createCheckpointAndClose)
import Data.ByteString                  (ByteString)
import Data.Default                     (Default(def))
import Data.FileEmbed                   (embedDir)
import Data.IxSet                       (IxSet, Indexable(empty), ixSet, ixFun, insert, getOne, getEQ, toDescList, Proxy(Proxy))
import Data.Lens                        (Lens, (^.), (%=), getL)
import Data.Lens.Template               (makeLens)
import Data.Map                         (Map, (!))
import Data.SafeCopy                    (base, deriveSafeCopy)
import Data.Text                        (Text, pack)
import Data.Text.Encoding               (encodeUtf8)
import Data.Typeable                    (Typeable)
import HSP.ServerPartT                  ()
import HSX.JMacro                       (IntegerSupply(nextInteger), nextInteger')
import Happstack.Server                 (ServerPartT, mapServerPartT, Response, ToMessage, toResponse, ok, setHeaderM, Input, simpleHTTP, nullConf, decodeBody, defaultBodyPolicy)
import Happstack.Server.FileServe       (guessContentTypeM, mimeTypes)
import Happstack.Server.HSP.HTML        (EmbedAsChild(asChild), EmbedAsAttr, cdata, genElement, asAttr, Attr((:=)), XMLGenT, unXMLGenT, XMLGenerator, genEElement)
import Happstack.Server.JMacro          ()
import Language.Javascript.JMacro
import Text.Blaze.Renderer.Text         (renderHtml)
import Text.Boomerang.TH                (derivePrinterParsers)
import Text.Digestive                   ((++>), Form, mapView)
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

askL :: MonadReader r m => Lens r t -> m t
askL = asks . getL

infixr 4 %.
(%.) :: MonadReader r m => Lens r t -> (t -> b) -> m b
(%.) = flip liftM . askL

recentPastes :: Query AppState [(Key,Paste)]
recentPastes = pastes %. take 10 . toDescList (Proxy :: Proxy Key)

savePaste :: Paste -> Update AppState Key
savePaste p = do
    k <- nextKey %= succ
    pastes %= insert (k,p)
    return k

getPaste :: Key -> Query AppState (Maybe Paste)
getPaste k = pastes %. fmap snd . getOne . getEQ k

makeAcidic ''AppState ['recentPastes, 'savePaste, 'getPaste]

class HasAcidState m st where
  getAcidState :: m (AcidState st)

query ::
    ( QueryEvent e
    , MonadIO m
    , HasAcidState m (MethodState e)
    ) => e -> m (EventResult e)
query ev = do
    st <- getAcidState
    query' st ev

update ::
    ( UpdateEvent e
    , MonadIO m
    , HasAcidState m (MethodState e)
    ) => e -> m (EventResult e)
update ev = do
    st <- getAcidState
    update' st ev

queryMaybe ::
    ( MethodResult e ~ Maybe a
    , QueryEvent e
    , MonadIO m
    , HasAcidState m (MethodState e)
    , MonadPlus m
    ) => e -> (a -> m b) -> m b
queryMaybe ev f = do
    m <- query ev
    maybe mzero f m


{---------------------------------------------------------------------------
 -                                 Routing                                 -
 ---------------------------------------------------------------------------}

data Sitemap = Asset FilePath | NewPaste | ShowPaste Key
derivePrinterParsers ''Sitemap

type Server = RouteT Sitemap (ServerPartT (ReaderT State (StateT Integer IO)))

instance HasAcidState Server AppState where
  getAcidState = ask

instance HasAcidState (XMLGenT Server) AppState where
  getAcidState = ask

sitemap :: Router Sitemap
sitemap = (rAsset . (lit "assets" </> anyString))
       <> (rNewPaste)
       <> (rShowPaste . integer)

site :: Site Sitemap (ServerPartT (ReaderT State (StateT Integer IO)) Response)
site = boomerangSiteRouteT route sitemap

route :: Sitemap -> Server Response

route (Asset f) = do
    mime <- guessContentTypeM mimeTypes f
    setHeaderM "Content-Type" mime
    ok $ toResponse $ assets ! f

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
          <% unit "7-24" recentPastesList %>
        </%>
      Right paste -> do
        k <- update $ SavePaste paste
        seeOtherURL $ ShowPaste k

route (ShowPaste k) =
    queryMaybe (GetPaste k) $ \paste -> do
      let text        = paste ^. content
          highlighted =
            case lexerFromFilename . T.unpack $ paste ^. fileName of
              Nothing    -> text
              Just lexer ->
                case runLexer lexer $ encodeUtf8 text of
                  Left _       -> text
                  Right tokens ->
                    L.toStrict . renderHtml $ format False tokens
      appTemplate
        <% unit "1"
          <%>
            <h2><% paste ^. fileName %></h2>
            <pre><% cdata . T.unpack $ highlighted %></pre>
          </%>
        %>


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

instance IntegerSupply Server where
  nextInteger = nextInteger'

appTemplate ::
    ( EmbedAsChild m (Lucius Sitemap)
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
        <% stylesheet "http://fonts.googleapis.com/css?family=Gloria+Hallelujah&text=Happaste" %>
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

recentPastesList :: XMLGenT Server [HSX.Child Server]
recentPastesList = do
    ps <- query RecentPastes
    asChild
      <ol class="recent-pastes">
        <% forM ps $ \(k,p) ->
          <li><a href=(ShowPaste k)><% p ^. fileName %></a></li>
        %>
      </ol>

css :: Lucius Sitemap
css = [$lucius|
  @width  : 60em;
  @gutter : 1em;
  @color1 : #3B4162;

  div.yui3-g div.unit
    { margin-left: #{gutter}
    }

  div.grid
    { max-width     : #{width}
    ; margin        : 0 auto
    ; padding-right : #{gutter}
    }

  div#header
    { background : #{color1}
    ; font-size : 123.1%
    ; h1
        { font-family : "Gloria Hallelujah", serif
        ; font-size   : 197%
        ; padding     : .5em 0
        ; margin      : 0
        ; a
            { color : #fff
            }
        }
    }

  div#content
    { font-size : 123.1%
    ; textarea
        { font-family : monospace
        ; width       : 100%
        ; height      : 2400%
        }
      a
        { color : #{color1}
        }
    }

  a
    { text-decoration : none
    }

  ol.recent-pastes
    { list-style-type : none
    ; padding-left    : 1em
    ; border-left     : .2em solid #{color1}
    }
|]

assets :: Map FilePath ByteString
assets = Map.fromList $(embedDir "assets")


{---------------------------------------------------------------------------
 -                                  Forms                                  -
 ---------------------------------------------------------------------------}

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


{---------------------------------------------------------------------------
 -                               Application                               -
 ---------------------------------------------------------------------------}

server :: State -> IO ()
server st = simpleHTTP nullConf $ do
    decodeBody $ defaultBodyPolicy "/tmp/" 0 40960 40960
    implSite (pack "http://localhost:8000") T.empty $
      fmap (mapServerPartT ((`evalStateT` 0) . (`runReaderT` st))) site

main :: IO ()
main = bracket (openLocalState def) createCheckpointAndClose server
