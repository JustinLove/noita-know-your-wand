module View exposing (Msg(..), document, view)

import Wand exposing (Wand, Dimension(..))

import Array
import Element exposing (..)
import Element.Border as Border
import Element.Input as Input
import Html exposing (Html)
import Html.Attributes

type Msg
  = None
  | ChangedRows Dimension
  | ChangedColumns Dimension
  | ChangedSort Dimension

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
      , dimensionSelect ChangedRows "Rows" model.rowDimension
      , dimensionSelect ChangedColumns "Columns" model.columnDimension
      , dimensionSelect ChangedSort "Sort" model.sortDimension
      , model.wands
        --|> List.take 20
        --|> List.singleton
        |> partitionTable model.rowDimension model.columnDimension model.sortDimension
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

dimensionSelect : (Dimension -> Msg) -> String -> Dimension-> Element Msg
dimensionSelect tagger title dim =
  Input.radioRow
    [
    ]
    { onChange = tagger
    , selected = Just dim
    , label = Input.labelRight [] (text title)
    , options =
      [ Input.option CastDelay (text "Cast Delay")
      , Input.option Actions (text "Actions")
      , Input.option Shuffle (text "Shuffle")
      , Input.option Slots (text "Slots")
      , Input.option Spread (text "Spread")
      , Input.option ReloadTime (text "ReloadTime")
      ]
    }

partitionTable : Dimension -> Dimension -> Dimension -> List Wand -> List (List (List Wand))
partitionTable rowDimension columnDimension sortDimension wands =
  wands
    |> partitionByNumber rowDimension
    |> List.map (partitionByNumber columnDimension)
    |> List.map (List.map (sortByDimension sortDimension))

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

sortByDimension : Dimension -> List Wand -> List Wand
sortByDimension dim wands =
  List.sortBy (Wand.attribute dim) wands
