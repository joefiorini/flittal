module Partials.About exposing (..)

import Markdown


view =
    Markdown.toHtml """

## Welcome to Flittal

#### Planning at the speed of thought

I've used Visio, OmniGraffle, Google Charts, etc, but none of them supported the fast paced workflow I needed for getting ideas out of my head. After enough time with suboptimal tools, I've decided to write my own.

Flittal is a lightweight, keyboard-driven tool to help you communicate your ideas. At the moment I've chosen to focus on flow charts, but the future may bring additional features and charts.

Get started by pressing `?` to browse the keyboard shortcuts that are available. When you are ready to share, click on the "Share this board" field.

### This tool is alpha

If you get frustrated with this tool or have ideas you'd like to see implemented, my email is <joe@joefiorini.com>, I want to hear from you.

Thanks for trying it out!

"""
