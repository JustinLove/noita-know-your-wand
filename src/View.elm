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
import PivotTable

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
      , displayTable model
      ]

displayTable model =
  PivotTable.pivotTable
    { rowGroupFields = List.map dimensionLabel model.rowDimension
    , colGroupFields = List.map dimensionLabel model.columnDimension
    , aggregator = \wands -> List.foldr sortByDimension wands model.sortDimension
    , viewRow = displayRowLabel
    , viewCol = displayColumnLabel
    , viewAgg = displayWandList
    }
    (PivotTable.makeTable model.wands)

dimensionLabel : Dimension -> (Wand -> String)
dimensionLabel dim =
  (Wand.attribute dim) >> (\i ->
    Wand.values dim
      |> List.drop i
      |> List.head
      |> Maybe.withDefault ("invalid value "++(String.fromInt i))
    )

displayColumnLabel : String -> Element Msg
displayColumnLabel name =
  el
    [ width fill
    , height fill
    , Border.widthEach
      { bottom = 0
      , left = 1
      , right = 0
      , top = 0
      }
    ]
    (el [ centerX, centerY ] (text name))

displayRowLabel : String -> Element Msg
displayRowLabel name =
  el
    [ width fill
    , height fill
    , Border.widthEach
      { bottom = 0
      , left = 0
      , right = 0
      , top = 1
      }
    ]
    (el
      [ width fill
      , height fill
      , padding 5
      ]
      (el [ centerX, centerY ] (text name))
    )

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

sortByDimension : Dimension -> List Wand -> List Wand
sortByDimension dim wands =
  List.sortBy (Wand.attribute dim) wands
