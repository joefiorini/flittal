module Routes exposing (..)

import Debug


type RouteName
    = Root
    | About
    | Colophon
    | Releases
    | Help
    | None


type alias Url =
    String


type alias Route =
    ( Url, RouteName )


map : (RouteName -> Route) -> Signal RouteName -> Signal Route
map =
    Signal.map


sendToPort : Signal Route -> Signal Url
sendToPort routeSignal =
    Signal.map Tuple.first routeSignal
