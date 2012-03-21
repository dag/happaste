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
import Data.Acid                        (AcidState, IsAcidic, QueryEvent, UpdateEvent, Query, Update, EventResult, makeAcidic, openLocalState)
import Data.Acid.Advanced               (MethodState, MethodResult, query', update')
import Data.Acid.Local                  (createCheckpointAndClose)
import Data.ByteString                  (ByteString)
import Data.Default                     (Default(def))
import Data.FileEmbed                   (embedDir)
import Data.IxSet                       (IxSet, Indexable(empty), ixSet, ixFun, insert, getOne, getEQ, toDescList, Proxy(Proxy))
import Data.Lens                        (Lens, (^.), (%=), getL)
import Data.Lens.Template               (makeLens)
import Data.Map                         (Map)
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

data PasteState = PasteState
  { _nextKey :: Key
  , _pastes  :: IxSet (Key,Paste)
  } deriving Typeable
makeLens ''PasteState
deriveSafeCopy 0 'base ''PasteState

instance Default PasteState where
  def = PasteState def def

data HighlighterState = HighlighterState
  { _highlights :: Map Key Text
  } deriving Typeable
makeLens ''HighlighterState
deriveSafeCopy 0 'base ''HighlighterState

instance Default HighlighterState where
  def = HighlighterState def

askL :: MonadReader r m => Lens r t -> m t
askL = asks . getL

infixr 4 %.
(%.) :: MonadReader r m => Lens r t -> (t -> b) -> m b
(%.) = flip liftM . askL

recentPasteState :: Query PasteState [(Key,Paste)]
recentPasteState = pastes %. take 10 . toDescList (Proxy :: Proxy Key)

savePaste :: Paste -> Update PasteState Key
savePaste p = do
    k <- nextKey %= succ
    pastes %= insert (k,p)
    return k

getPaste :: Key -> Query PasteState (Maybe Paste)
getPaste k = pastes %. fmap snd . getOne . getEQ k

makeAcidic ''PasteState ['recentPasteState, 'savePaste, 'getPaste]

saveHighlight :: Key -> Text -> Update HighlighterState Text
saveHighlight k t = do
    highlights %= Map.insert k t
    return t

getHighlight :: Key -> Query HighlighterState (Maybe Text)
getHighlight k = highlights %. Map.lookup k

makeAcidic ''HighlighterState ['saveHighlight, 'getHighlight]

data States = States
  { _pasteState       :: AcidState PasteState
  , _highlighterState :: AcidState HighlighterState
  }
makeLens ''States

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

type Server = RouteT Sitemap (ServerPartT (ReaderT States (StateT Integer IO)))

instance HasAcidState Server PasteState where
  getAcidState = askL pasteState

instance HasAcidState (XMLGenT Server) PasteState where
  getAcidState = askL pasteState

instance HasAcidState Server HighlighterState where
  getAcidState = askL highlighterState

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
    case Map.lookup f assets of
      Nothing -> mzero
      Just bs -> ok $ toResponse bs

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
      highlighted <- highlight k (T.unpack $ p ^. fileName) $ p ^. content
      appTemplate $ unit "1"
        <%>
          <h2><% p ^. fileName %></h2>
          <% cdata . T.unpack $ highlighted %>
        </%>

highlight ::
    ( HasAcidState m HighlighterState
    , MonadIO m
    ) => Key -> FilePath -> Text -> m Text
highlight k f t = do
    h <- query $ GetHighlight k
    case h of
      Just t' -> return t'
      Nothing ->
        update $ SaveHighlight k $
          case lexerFromFilename f of
            Nothing -> t
            Just l  ->
              case runLexer l $ encodeUtf8 t of
                Left _   -> t
                Right ts -> L.toStrict . renderHtml $ format False ts


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

recentPasteStateList :: XMLGenT Server [HSX.Child Server]
recentPasteStateList = do
    ps <- query RecentPasteState
    asChild
      <ol class="recent-pastes">
        <% forM ps $ \(k,p) ->
          <li><a href=(ShowPaste k)><% p ^. fileName %></a></li>
        %>
      </ol>

css :: Lucius Sitemap
css = [$lucius|
  @width           : 60em;
  @gutter          : 1em;
  @color1          : #327CCB;
  @color1_gradient : #3070C0;

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
    ; background : -moz-linear-gradient(top, #{color1}, #{color1_gradient})
    ; background : -webkit-linear-gradient(top, #{color1}, #{color1_gradient})
    ; background : -webkit-gradient(linear, 0% 0%, 0% 100%,
                                    from(#{color1}), to(#{color1_gradient}))
    ; background : -ms-linear-gradient(top, #{color1}, #{color1_gradient})
    ; background : -o-linear-gradient(top, #{color1}, #{color1_gradient})
    ; h1
        { padding     : .5em 0
        ; margin      : 0
        ; a
            { color : #fff
            }
        }
    }

  div#content
    { margin-bottom : 1em
    ; textarea
        { font-family : monospace
        ; width       : 100%
        ; height      : 2400%
        }
      input[type=submit]
        { float : right
        }
      a
        { color : #{color1}
        }
      *
        { line-height : 150%
        }
    }

  h1,h2,h3,h4,h5,h6
    { font-family : "Stoke", serif
    ; font-weight : 400
    }

  a
    { text-decoration : none
    }

  ol.recent-pastes
    { list-style-type : none
    ; padding-left    : 1em
    ; border-left     : .2em solid #{color1}
    }

  div.highlight
    { background        : #fafafa
    ; border            : .1em solid #eee;
    ; border-left-width : .4em
    ; pre
        { padding : 1em 1em 1em 1.3em
        ; margin  : 0
        }
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

server :: AcidState PasteState -> AcidState HighlighterState -> IO ()
server ps hs = simpleHTTP nullConf $ do
    decodeBody $ defaultBodyPolicy "/tmp/" 0 40960 40960
    implSite (pack "http://localhost:8000") T.empty $
      fmap (mapServerPartT ((`evalStateT` 0) . (`runReaderT` st))) site
  where
    st = States ps hs

withState ::
    ( Default st
    , IsAcidic st
    , Typeable st
    ) => (AcidState st -> IO ()) -> IO ()
withState = bracket (openLocalState def) createCheckpointAndClose

main :: IO ()
main = withState $ withState . server
