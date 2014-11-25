module Connection.Controller where

import Html (Html)
import Html.Tags (div)
import Html.Attributes (class)

import Connection.State (Connection)

type State = Connection

renderConnection : Connection -> Html
renderConnection connection = div [
                      class "connection"
                    ] []
