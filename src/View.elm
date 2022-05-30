module View exposing (Msg(..), document, view)

import Element exposing (..)
import Html exposing (Html)

type Msg
  = None

document tagger model =
  { title = "Noita, Know Your Wand"
  , body = [Html.map tagger (view model)]
  }

view model =
  layout [] <|
    (text "Noita, know your wand")
