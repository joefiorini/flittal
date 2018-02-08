module Partials.Releases exposing (..)

import Markdown


view =
    Markdown.toHtml """

## What's New

### [1.1.1](https://github.com/joefiorini/flittal/releases/1.1.0)

Third release in just as many years, codenamed "Better Late Than Never". This release fixes the long broken Share feature. You can now use the "Share this Board" field above the board to get a link to share or come back to your board. Two important notes:

  1. Your data is **NOT** sent to any servers; all your data is in the share URL
  2. If you leave the site or refresh the page, you will lose your board, so be careful!

### [1.1.0](https://github.com/joefiorini/flittal/releases/1.1.0)

Second release brings Undo/Redo functionality. Press `u` to undo an action and `ctrl+r` to redo it. I also fixed some bugs related to deleting multiple selections at once, and a bug around disconnecting boxes. I also added this "What's New" sidebar so you can keep track of recent changes.

#### Coming Soon

- Titles for boards

### [1.0.0](https://github.com/joefiorini/flittal/releases/1.0.0)

Initial release with ability to create a basic flow chart with rectangular shapes. Boxes can be manipulated in a variety of ways: move, resize, change their text, etc. See the Help link in the header for instructions and more information.

#### Coming Soon

- Undo
- Give boards a title

"""
