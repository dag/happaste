module Happaste.State where

import qualified Data.Map as Map

import Data.Acid  (Query, Update, makeAcidic)
import Data.IxSet (insert, getOne, getEQ, toDescList, Proxy(Proxy))
import Data.Lens  ((%=))
import Data.Text  (Text)

import Happaste.Types

recentPastes :: Query PasteState [(Key,Paste)]
recentPastes = pastes %. take 10 . toDescList (Proxy :: Proxy Key)

savePaste :: Paste -> Update PasteState Key
savePaste p = do
    k <- nextKey %= succ
    pastes %= insert (k,p)
    return k

getPaste :: Key -> Query PasteState (Maybe Paste)
getPaste k = pastes %. fmap snd . getOne . getEQ k

makeAcidic ''PasteState ['recentPastes, 'savePaste, 'getPaste]

saveHighlight :: Key -> Text -> Update HighlighterState Text
saveHighlight k t = do
    highlights %= Map.insert k t
    return t

getHighlight :: Key -> Query HighlighterState (Maybe Text)
getHighlight k = highlights %. Map.lookup k

makeAcidic ''HighlighterState ['saveHighlight, 'getHighlight]
