module Routes exposing (..)


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
