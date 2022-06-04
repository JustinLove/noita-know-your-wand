module View exposing (Msg(..), document, view, Expression(..))

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
  | ChangedExpression Dimension Expression

type Expression
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
      , expressionSelect (ChangedExpression CastDelay) "Cast Delay" (currentExpression model CastDelay)
      , expressionSelect (ChangedExpression Actions) "Actions" (currentExpression model Actions)
      , expressionSelect (ChangedExpression Shuffle) "Shuffle" (currentExpression model Shuffle)
      , expressionSelect (ChangedExpression Slots) "Slots" (currentExpression model Slots)
      , expressionSelect (ChangedExpression Spread) "Spread" (currentExpression model Spread)
      , expressionSelect (ChangedExpression ReloadTime) "Reload Time" (currentExpression model ReloadTime)
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

expressionSelect : (Expression -> Msg) -> String -> Expression -> Element Msg
expressionSelect tagger title exp =
  Input.radioRow
    [ spacing 10 
    ]
    { onChange = tagger
    , selected = Just exp
    , label = Input.labelRight [] (text title)
    , options =
      [ Input.option Rows (text "Rows")
      , Input.option Columns (text "Columns")
      , Input.option Sort (text "Sort")
      ]
    }

--currentExpression : Model -> Dimension -> Expression
currentExpression model dim =
  if List.member dim model.rowDimension then
    Rows
  else if List.member dim model.columnDimension then
    Columns
  else
    Sort

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
