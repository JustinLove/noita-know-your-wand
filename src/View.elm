module View exposing (Msg(..), document, view)

import Wand exposing (Wand, Dimension(..))

import Array
import Element exposing (..)
import Element.Border as Border
import Html exposing (Html)
import Html.Attributes

type Msg
  = None

document tagger model =
  { title = "Noita, Know Your Wand"
  , body = [Html.map tagger (view model)]
  }

view model =
  layout
    [ width fill
    ] <|
    column
      [ width fill
      ]
      [ (text "Noita, know your wand")
      , model.wands
        --|> List.take 20
        --|> List.singleton
        |> partitionTable model.rowDimension model.columnDimension
        |> displayWandTable
      ]

displayWandTable : List (List (List Wand)) -> Element Msg
displayWandTable wands =
  column
    [ width fill
    ]
    (List.map displayWandRow wands)

displayWandRow : List (List Wand) -> Element Msg
displayWandRow wands =
  row
    [ width fill
    ]
    (List.map displayWandList wands)

displayWandList : List Wand -> Element Msg
displayWandList wands =
  wrappedRow
    [ Border.width 1
    , width fill
    , alignTop
    ]
    (List.map displayWand wands)

displayWand : Wand -> Element Msg
displayWand wand =
  row
    [ height (px 20)
    , width (px 50)
    ]
    [ image
      [ htmlAttribute <| Html.Attributes.class "wand-sprite"
      , htmlAttribute <| Html.Attributes.class "crisp"
      ]
      { src = wand.file
      , description = wand.file
      }
    ]

partitionTable : Dimension -> Dimension -> List Wand -> List (List (List Wand))
partitionTable rowDimension columnDimension wands =
  wands
    |> partitionByNumber rowDimension
    |> List.map (partitionByNumber columnDimension)

partitionByNumber : Dimension -> List Wand -> List (List Wand)
partitionByNumber dim wands =
  List.foldl (\wand sorted ->
      let
        value = (Wand.attribute dim) wand
      in
        Array.get value sorted
          |> Maybe.map (\list -> wand :: list)
          |> Maybe.withDefault [wand]
          |> (\list -> Array.set value list sorted)
    ) (Array.initialize (List.length (Wand.values dim)) (always [])) wands
  |> Array.toList
