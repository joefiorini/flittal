module Routes where

import Debug
import Signal

type RouteName = Root
               | About
               | Colophon
               | Help

type alias Url = String
type alias Route = (Url, RouteName)


map : (RouteName -> Route) -> Signal RouteName -> Signal Route
map = Signal.map

sendToPort : Signal Route -> Signal Url
sendToPort routeSignal =
  Signal.map fst routeSignal
