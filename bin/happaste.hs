{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text as T

import Control.Exception    (bracket)
import Control.Monad        (msum)
import Control.Monad.Reader (runReaderT)
import Control.Monad.State  (evalStateT)
import Data.Acid            (AcidState, IsAcidic, openLocalState)
import Data.Acid.Local      (createCheckpointAndClose)
import Data.Default         (Default(def))
import Data.Typeable        (Typeable)
import Happstack.Server     (mapServerPartT, simpleHTTP, nullConf, decodeBody, defaultBodyPolicy)
import Happstack.Server.YUI (implYUISite)
import System.Log.Logger    (updateGlobalLogger, rootLoggerName, setLevel, Priority(DEBUG))
import Web.Routes.Happstack (implSite)

import Happaste.HTTP
import Happaste.Types

server :: AcidState PasteState -> AcidState HighlighterState -> IO ()
server ps hs = simpleHTTP nullConf $
    msum [ implYUISite "http://localhost:8000" "/yui"
         , do decodeBody $ defaultBodyPolicy "/tmp/" 0 40960 40960
              implSite "http://localhost:8000" T.empty $
                fmap (mapServerPartT ((`evalStateT` 0) . (`runReaderT` st))) site
         ]
  where
    st = States ps hs

withState ::
    ( Default st
    , IsAcidic st
    , Typeable st
    ) => (AcidState st -> IO ()) -> IO ()
withState = bracket (openLocalState def) createCheckpointAndClose

main :: IO ()
main = do
    updateGlobalLogger rootLoggerName $ setLevel DEBUG
    withState $ withState . server
