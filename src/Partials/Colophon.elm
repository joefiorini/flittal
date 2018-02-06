module Partials.Colophon exposing (..)

import Markdown


view =
    Markdown.toHtml """

## What I Used

This app was designed and built by Joe Fiorini.

- [Sketch](http://bohemiancoding.com/sketch/) for design
- [Elm](http://elm-lang.org) for [beautiful code](https://github.com/joefiorini/flittal)
- [Vim](http://www.vim.org) for writing

Thanks to the [elm-discuss](https://groups.google.com/forum/#!forum/elm-discuss) mailing list for answering my questions.

Dedicated to Katie for giving me the time to do this even when she didn't want to ;)

"""
