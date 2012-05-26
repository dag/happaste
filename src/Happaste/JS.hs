{-# OPTIONS_GHC -F -pgmF trhsx #-}

module Happaste.JS where

import Language.Javascript.JMacro

pjax :: JStat
pjax = [jmacro| YUI().use "pjax" \y ->
                  new y.Pjax { container    : "#content"
                             , linkSelector : "a.pjax"
                             } |]
