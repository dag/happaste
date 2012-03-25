module Happaste.Css where

import Prelude (($), (++))

import Language.Css.Build
import Language.Css.Build.Attributes (type')
import Language.Css.Build.Idents hiding (content, pre)
import Language.Css.Build.Tags hiding (em, header)
import Language.Css.Syntax

gridWidth :: Expr
gridWidth = em 60

gutter :: Expr
gutter = em 1

color1 :: Expr
color1 = cword "#327CCB"

color1Gradient :: Expr
color1Gradient = cword "#3070C0"

grid :: RuleSet
grid = div /. "grid" $
    [ maxWidth     <:> gridWidth
    , margin       <:> int 0 `space` auto
    , paddingRight <:> gutter
    ]

unit :: RuleSet
unit = div /. "yui3-g" /- div /. "unit" $
    [ marginLeft <:> gutter
    ]

typography :: [RuleSet]
typography =
    [ sels [h1,h2,h3,h4,h5,h6] $
        [ fontFamily <:> str "Stoke" `comma` serif
        , fontWeight <:> int 400
        ]
    , a $
        [ textDecoration <:> none
        ]
    ]

header :: [RuleSet]
header =
    [ div /# "header" $
        linearGradient color1 color1Gradient
    , div /# "header" /- h1 $
        [ padding <:> em 0.5 `space` int 0
        , margin  <:> int 0
        ]
    , div /# "header" /- star $
        [ color <:> cword "#fff"
        ]
    ]

content :: [RuleSet]
content =
    [ div /# "content" $
        [ marginBottom <:> em 1
        ]
    , div /# "content" /- star $
        [ lineHeight <:> pct 150
        ]
    , div /# "content" /- a $
        [ color <:> color1
        ]
    , div /# "content" /- textarea $
        [ fontFamily <:> monospace
        , width      <:> pct 100
        , height     <:> pct 2400
        ]
    , div /# "content" /- input ! type' .= "submit" $
        [ float <:> right
        ]
    , ol /. "recent-pastes" $
        [ listStyleType <:> none
        , paddingLeft   <:> em 1
        , borderLeft    <:> spaces [em 0.2, solid, color1]
        ]
    , div /. "highlight" $
        [ background      <:> cword "#fafafa"
        , border          <:> spaces [em 0.1, solid, cword "#eee"]
        , borderLeftWidth <:> em 0.4
        , overflow        <:> auto
        ]
    , div /. "highlight" /- pre $
        [ padding <:> spaces [em 1, em 1, em 1, em 1.3]
        , margin  <:> int 0
        ]
    ]

css :: StyleSheet
css = ruleSets $ [grid, unit] ++ typography ++ header ++ content

linearGradient :: Expr -> Expr -> [Decl]
linearGradient from to =
    [ background <:> from
    , background <:> func "-moz-linear-gradient" [top, from, to]
    , background <:> func "-ms-linear-gradient" [top, from, to]
    , background <:> func "-o-linear-gradient" [top, from, to]
    , background <:> func "-webkit-gradient" webkit
    , background <:> func "-webkit-linear-gradient" [top, from, to]
    ]
  where
    func name args = expr $ ident name `fun` commas args
    webkit =
      [ ident "linear"
      , pct 0 `space` pct 0
      , pct 0 `space` pct 100
      , expr (ident "from" `fun` from)
      , expr (ident "to" `fun` to)
      ]
