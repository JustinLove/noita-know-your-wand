module View exposing (Msg(..), document, view)

import Wand exposing (Wand, Dimension(..))

import Array
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Input as Input
import Html exposing (Html)
import Html.Attributes

type Msg
  = None
  | ChangedRows Dimension
  | ChangedColumns Dimension
  | ChangedSort Dimension

type Expressions
  = Rows
  | Columns
  | Sort

document tagger model =
  { title = "Noita, Know Your Wand"
  , body = [Html.map tagger (view model)]
  }

view model =
  layout
    [ width fill
    , Background.color (rgb 0.2 0.2 0.2)
    ] <|
    column
      [ width fill
      ]
      [ (text "Noita, know your wand")
      , dimensionSelect ChangedRows "Rows" (List.head model.rowDimension |> Maybe.withDefault Slots)
      , dimensionSelect ChangedColumns "Columns" (List.head model.columnDimension |> Maybe.withDefault Slots)
      , dimensionSelect ChangedSort "Sort" (List.head model.sortDimension |> Maybe.withDefault Slots)
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
    [ spacing 10 
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

partitionTable : List Dimension -> List Dimension -> List Dimension -> List Wand -> List (List (List Wand))
partitionTable rowDimensions columnDimensions sortDimensions wands =
  wands
    |> multiPartition rowDimensions
    |> List.map (multiPartition columnDimensions)
    |> List.map (List.map (\list -> List.foldr sortByDimension list sortDimensions))

multiPartition : List Dimension -> List Wand -> List (List Wand)
multiPartition dimensions wands =
  case dimensions of
    dimension :: rest ->
      partitionByNumber dimension wands
        |> List.concatMap (multiPartition rest)
    [] ->
      [wands]

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
