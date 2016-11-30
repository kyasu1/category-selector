module Main exposing (..)

import Html
import View exposing (view)
import Category exposing (update, initialModel)


main =
    Html.program
        { init = initialModel
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }
