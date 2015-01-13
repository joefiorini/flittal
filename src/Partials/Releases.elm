module Partials.Releases where

import Markdown

view =
  Markdown.toHtml """

## What's New

### [1.0.0](https://github.com/joefiorini/flittal/releases/1.0.0)

Initial release with ability to create a basic flow chart with rectangular shapes. Boxes can be manipulated in a variety of ways: move, resize, change their text, etc. See the Help link in the header for instructions and more information.

#### Coming Soon

- Undo
- Give boards a title

"""
