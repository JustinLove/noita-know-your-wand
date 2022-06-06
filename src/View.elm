module View exposing (Msg(..), document, view, Expression(..), DropTarget(..))

import Wand exposing (Wand, Dimension(..))

import Array
import Dom
import Dom.DragDrop as DragDrop
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Input as Input
import Html exposing (Html)
import Html.Attributes

type Msg
  = None
  | DragStarted Dimension
  | DragTargetChanged DropTarget
  | DragCanceled
  | DragCompleted Dimension DropTarget

dragMessages : DragDrop.Messages Msg Dimension DropTarget
dragMessages =
  { dragStarted = DragStarted
  , dropTargetChanged = DragTargetChanged
  , dragEnded = DragCanceled
  , dropped = DragCompleted
  }

type Expression
  = Rows
  | Columns
  | Sort

type DropTarget
  = OntoElement Expression Dimension
  | EndOfList Expression

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
      , expressionBox Rows "Rows" model.rowDimension model.dragDropState
      , expressionBox Columns "Columns" model.columnDimension model.dragDropState
      , expressionBox Sort "Sort" model.sortDimension model.dragDropState
      , row [ width fill ]
        [ column
          [ height fill ]
          [ el
            [ height (px ((List.length model.columnDimension) * 20))
            ]
            none
          , model.rowDimension
              |> displayRowHeaders
          ]
        , column
          [ width fill
          ]
          [ model.columnDimension
            |> displayColumnHeaders
          , model.wands
            --|> List.take 20
            --|> List.singleton
            |> partitionTable model.rowDimension model.columnDimension model.sortDimension
            |> displayWandTable
          ]
        ]
      ]

displayColumnHeaders : List Dimension -> Element Msg
displayColumnHeaders dimensions =
  column
    [ width fill
    ]
    ( case dimensions of
        head :: rest ->
          [(displayColumnHeader rest head)]
        [] ->
          []
    )

displayColumnHeader : List Dimension -> Dimension -> Element Msg
displayColumnHeader below dim =
  row
    [ width fill
    ]
    (List.map (displayColumnLabelAndBelow below) (Wand.values dim))

displayColumnLabelAndBelow : List Dimension -> String -> Element Msg
displayColumnLabelAndBelow below name =
  column
    [ width fill ]
    [ displayLabel name
    , displayColumnHeaders below
    ]

displayRowHeaders : List Dimension -> Element Msg
displayRowHeaders dimensions =
  row
    [ height fill
    ]
    ( case dimensions of
        head :: rest ->
          [(displayRowHeader rest head)]
        [] ->
          []
    )

displayRowHeader : List Dimension -> Dimension -> Element Msg
displayRowHeader below dim =
  column
    [ height fill
    ]
    (List.map (displayRowLabelAndBelow below) (Wand.values dim))

displayRowLabelAndBelow : List Dimension -> String -> Element Msg
displayRowLabelAndBelow below name =
  row
    [ height fill ]
    [ displayLabel name
    , displayRowHeaders below
    ]

displayLabel : String -> Element Msg
displayLabel name =
  el
    [ width (minimum 50 fill)
    , height (minimum 20 fill)
    ]
    (text name)

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
    , width (minimum 50 fill)
    --, height (minimum 20 fill)
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

displayDimension : Dimension -> Element Msg
displayDimension dim =
  el
    [ Background.color (rgb 0.5 0.5 0.5)
    , padding 10
    ]
    (text (Wand.name dim))

domDimension : Dimension -> Dom.Element Msg
domDimension dim =
  Dom.element "div"
    |> Dom.appendText (Wand.name dim)
    |> Dom.addClass "dom-dimension"

domEndOfList : Dom.Element Msg
domEndOfList =
  Dom.element "div"
    |> Dom.addClass "dom-end"

uiToDom : Element Msg -> Dom.Element Msg
uiToDom el =
  Dom.element "div"
    |> Dom.appendNode (layout [] el)

domToUi : Dom.Element Msg -> Element Msg
domToUi dom =
  Dom.render dom
    |> html

draggableDimension : DragDrop.State Dimension DropTarget -> Expression -> Dimension -> Element Msg
draggableDimension state exp dim =
  (domDimension dim)
    |> DragDrop.makeDraggable state dim dragMessages
    |> DragDrop.makeDroppable state (OntoElement exp dim) dragMessages
    |> domToUi
    |> el []

draggableEndOfList : DragDrop.State Dimension DropTarget -> Expression -> Element Msg
draggableEndOfList state exp =
  domEndOfList
    |> DragDrop.makeDroppable state (EndOfList exp) dragMessages
    |> domToUi
    |> el [ width fill ]

expressionBox : Expression -> String -> List Dimension -> DragDrop.State Dimension DropTarget -> Element Msg
expressionBox exp title dims state =
  column [ width fill ]
    [ text title
    , row
      [ Border.width 1
      , height (px 50)
      , spacing 10
      , width fill
      ]
      ((List.map (draggableDimension state exp) dims) ++ [draggableEndOfList state exp])
    ]

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
