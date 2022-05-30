module View exposing (Msg(..), document, view, Wand)

import Element exposing (..)
import Html exposing (Html)
import Html.Attributes

type Msg
  = None

type alias Wand =
  { file : String
  , gripX : Int
  , gripY : Int
  , tipX : Int
  , tipY : Int
  , castDelay : Int
  , actions : Int
  , shuffle : Bool
  , deckCapacity : Int
  , spread : Int
  , reloadTime : Int
  }


document tagger model =
  { title = "Noita, Know Your Wand"
  , body = [Html.map tagger (view model)]
  }

view model =
  layout [] <|
    column []
      [ (text "Noita, know your wand")
      , model.wands
        --|> List.take 20
        |> displayWandList
      ]

displayWandList : List Wand -> Element Msg
displayWandList wands =
  column []
    (List.map displayWand wands)

displayWand : Wand -> Element Msg
displayWand wand =
  row
    [ height (px 40)
    ]
    [ image
      [ htmlAttribute <| Html.Attributes.class "wand-sprite"
      , htmlAttribute <| Html.Attributes.class "crisp"
      ]
      { src = wand.file
      , description = wand.file
      }
    ]
