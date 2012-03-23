module Happaste.Css where

import Text.Lucius (lucius)

import Happaste.Types

css :: Lucius Sitemap
css = [lucius|
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
