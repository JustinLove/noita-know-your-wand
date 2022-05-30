module View exposing (Msg(..), document, view, Wand)

import Array
import Element exposing (..)
import Element.Border as Border
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
  , shuffle : Int
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
        --|> List.singleton
        --|> partitionByNumber .deckCapacity 8
        |> partitionByNumber .castDelay 3
        --|> partitionByNumber .reloadTime 3
        --|> partitionByNumber .actions 3
        --|> partitionByNumber .spread 3
        --|> partitionByNumber .shuffle 2
        |> displayPartitionedWandList
      ]

displayPartitionedWandList : List (List Wand) -> Element Msg
displayPartitionedWandList wands =
  column []
    (List.map displayWandList wands)

displayWandList : List Wand -> Element Msg
displayWandList wands =
  wrappedRow
    [ Border.width 1
    ]
    (List.map displayWand wands)

displayWand : Wand -> Element Msg
displayWand wand =
  row
    [ height (px 40)
    , width (px 100)
    ]
    [ image
      [ htmlAttribute <| Html.Attributes.class "wand-sprite"
      , htmlAttribute <| Html.Attributes.class "crisp"
      ]
      { src = wand.file
      , description = wand.file
      }
    ]

partitionByNumber : (Wand -> Int) -> Int -> List Wand -> List (List Wand)
partitionByNumber attr max wands =
  List.foldl (\wand sorted ->
      let
        value = attr wand
      in
        Array.get value sorted
          |> Maybe.map (\list -> wand :: list)
          |> Maybe.withDefault [wand]
          |> (\list -> Array.set value list sorted)
    ) (Array.initialize max (always [])) wands
  |> Array.toList
