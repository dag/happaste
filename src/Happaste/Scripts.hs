{-# OPTIONS_GHC -F -pgmF trhsx #-}

module Happaste.Scripts where

import qualified HSP.Identity as HSP

import Happstack.Server.HSP.HTML
import Language.Javascript.JMacro

import Happaste.Types ()

yuiNode :: HSP.Ident XML -> JExpr -> JExpr
yuiNode x y = [$jmacroE| `(y)`.Node.create(`(x)`) |]

pjax :: JStat
pjax =
  [$jmacro|
    YUI().use "pjax" \y ->
      new y.Pjax { container: "#content"
                 , linkSelector: "a.pjax"
                 }
  |]
