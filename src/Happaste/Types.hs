{-# OPTIONS_GHC -F -pgmF trhsx #-}

module Happaste.Types where

import qualified Data.Text   as T
import qualified Text.Reform as Re

import Control.Monad             (MonadPlus, mzero, liftM)
import Control.Monad.Reader      (MonadReader, asks, ReaderT)
import Control.Monad.State       (StateT)
import Control.Monad.Trans       (MonadIO)
import Data.Acid                 (AcidState, QueryEvent, UpdateEvent, EventResult)
import Data.Acid.Advanced        (MethodState, MethodResult, query', update')
import Data.Default              (Default(def))
import Data.IxSet                (IxSet, Indexable(empty), ixSet, ixFun)
import Data.Lens                 (Lens, (^.), getL)
import Data.Lens.Template        (makeLens)
import Data.Map                  (Map)
import Data.SafeCopy             (base, deriveSafeCopy)
import Data.Text                 (Text)
import Data.Typeable             (Typeable)
import Happstack.Server          (ServerPartT, Input)
import Happstack.Server.HSP.HTML (EmbedAsChild(asChild), EmbedAsAttr(asAttr), genElement, asAttr, Attr((:=)), XMLGenT)
import Happstack.Server.JMacro   ()
import HSX.JMacro                (IntegerSupply(nextInteger), nextInteger')
import HSX.XMLGenerator          (XMLType)
import Language.Css.Pretty       (prettyPrint)
import Language.Css.Syntax       (StyleSheet)
import Text.Boomerang.TH         (derivePrinterParsers)
import Text.Reform               (ErrorInputType)
import Web.Routes                (RouteT)
import Web.Routes.Happstack      ()
import Web.Routes.XMLGenT        ()

summon :: MonadReader r m => Lens r t -> m t
summon = asks . getL

infixr 4 %.
(%.) :: MonadReader r m => Lens r t -> (t -> b) -> m b
(%.) = flip liftM . summon

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

data Sitemap = AssetURL FilePath | CreatePasteURL | GetPasteURL Key
derivePrinterParsers ''Sitemap

type Server = RouteT Sitemap (ServerPartT (ReaderT States (StateT Integer IO)))

instance HasAcidState Server PasteState where
  getAcidState = summon pasteState

instance HasAcidState (XMLGenT Server) PasteState where
  getAcidState = summon pasteState

instance HasAcidState Server HighlighterState where
  getAcidState = summon highlighterState

instance IntegerSupply Server where
  nextInteger = nextInteger'

instance EmbedAsChild Server StyleSheet where
  asChild style =
    <%>
      <style type="text/css">
        <% prettyPrint style %>
      </style>
    </%>

type Template = XMLGenT Server (XMLType Server)

data FormError = CommonFormError (Re.CommonFormError [Input])

instance Re.FormError FormError where
    type ErrorInputType FormError = [Input]
    commonFormError = CommonFormError

type Form = Re.Form Server [Input] FormError [Template] ()
