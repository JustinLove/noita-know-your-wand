module NoitaKnowYourWand exposing (..)

--import Log
import View exposing (Expression(..), DropTarget(..), Focus(..), Quadrant(..), dnd)
import Wand exposing (Wand, Dimension(..))
import Wand.Generated
--import Wand.Live

import Browser
import Browser.Dom as Dom
import Browser.Events
import DnD
import Dom.DragDrop as DragDrop
import Http
import PivotTable
import Task
import Time

type Msg
  = UI (View.Msg)
  --| GotWands (Result Http.Error (List Wand))
  | WindowSize (Int, Int)
  | HoverTimeout Time.Posix

type alias Model =
  { wands : PivotTable.Table Wand
  , rowDimension : List Dimension
  , columnDimension : List Dimension
  , sortDimension : List Dimension
  , dragDropState : DragDrop.State Dimension DropTarget
  , draggable : DnD.Draggable DropTarget Dimension
  , focusWand : Focus
  , showingControls : Bool
  , showingHeaders : Bool
  , windowWidth : Int
  , windowHeight : Int
  }

main = Browser.document
  { init = init
  , view = View.document UI
  , update = update
  , subscriptions = subscriptions
  }

init : () -> (Model, Cmd Msg)
init flags =
  ( { wands = PivotTable.makeTable Wand.Generated.wands
    , rowDimension = [CastDelay]
    , columnDimension = [Actions, Shuffle]
    , sortDimension = [Slots, Spread, ReloadTime]
    , dragDropState = DragDrop.initialState
    , draggable = dnd.model
    , focusWand = NoFocus
    , showingControls = True
    , showingHeaders = True
    , windowWidth = 320
    , windowHeight = 200
    }
  , Cmd.batch
    [ initialWindowSize
    --, Wand.Live.fetchWands GotWands
    ]
  )

initialWindowSize : Cmd Msg
initialWindowSize =
  Dom.getViewport
    |> Task.map (\viewport -> (round viewport.viewport.width, round viewport.viewport.height))
    |> Task.perform WindowSize

update msg model =
  case msg |> Debug.log "msg" of
    UI (View.None) ->
      (model, Cmd.none)
    UI (View.DragStarted dim) ->
      ( { model
        | dragDropState = DragDrop.startDragging model.dragDropState dim
        }
      , Cmd.none)
    UI (View.DragTargetChanged drop) ->
      ( { model
        | dragDropState = DragDrop.updateDropTarget model.dragDropState drop
        }
      , Cmd.none)
    UI (View.DragCanceled) ->
      ( { model
        | dragDropState = DragDrop.stopDragging model.dragDropState
        }
      , Cmd.none)
    UI (View.DragCompleted dim drop) ->
      ( { model | dragDropState = DragDrop.stopDragging model.dragDropState }
        |> removeFromExpressions dim
        |> dropOperation dim drop
      , Cmd.none
      )
    UI (View.Dropped drop dim) ->
      ( { model | dragDropState = DragDrop.stopDragging model.dragDropState }
        |> removeFromExpressions dim
        |> dropOperation dim drop
      , Cmd.none
      )
    UI (View.DnDMsg dndmsg) ->
      ( { model | draggable = DnD.update dndmsg model.draggable }, Cmd.none )
    UI (View.ToggleControls) ->
      ( { model | showingControls = not model.showingControls }, Cmd.none )
    UI (View.ToggleHeaders) ->
      ( { model | showingHeaders = not model.showingHeaders }, Cmd.none )
    UI (View.WandOver wand event) ->
      ( { model
        | focusWand =
          if floor (Tuple.first event.clientPos) < model.windowWidth // 2 then
            if floor (Tuple.second event.clientPos) < model.windowHeight // 2 then
              Enter LowerRight wand
            else
              Enter UpperRight wand
          else
            if floor (Tuple.second event.clientPos) < model.windowHeight // 2 then
              Enter LowerLeft wand
            else
              Enter UpperLeft wand
        }
      , Cmd.none
      )
    UI (View.WandOut wand event) ->
      ( {model | focusWand = NoFocus}, Cmd.none )
    {-GotWands (Ok wands) ->
      ( { model
        | wands = PivotTable.makeTable wands
        --, focusWand = List.head wands
        }
      , Cmd.none
      )
    GotWands (Err error) ->
      (model, Log.httpError "fetch error: wands" error)
      -}
    WindowSize (width, height) ->
      ( {model | windowWidth = width, windowHeight = height}
      , Cmd.none
      )
    HoverTimeout _ ->
      ( { model
        | focusWand =
          case model.focusWand of
            Focus _ _ -> model.focusWand
            Enter quad wand -> Focus quad wand
            NoFocus -> model.focusWand
        }
      , Cmd.none
      )

removeFromExpressions : Dimension -> Model -> Model
removeFromExpressions dim model =
  { model
  | rowDimension = List.filter (\x -> x /= dim) model.rowDimension
  , columnDimension = List.filter (\x -> x /= dim) model.columnDimension
  , sortDimension = List.filter (\x -> x /= dim) model.sortDimension
  }

dropOperation : Dimension -> DropTarget -> Model -> Model
dropOperation insert drop model =
  case drop of
    OntoElement exp before ->
      insertBefore exp insert before model
    EndOfList exp ->
      appendToExpression exp insert model

appendToExpression : Expression -> Dimension -> Model -> Model
appendToExpression exp dim model =
  case exp of
    Rows ->
      { model
      | rowDimension = listAppend dim model.rowDimension
      }
    Columns ->
      { model
      | columnDimension = listAppend dim model.columnDimension
      }
    Sort ->
      { model
      | sortDimension = listAppend dim model.sortDimension
      }

listAppend : a -> List a -> List a
listAppend x list =
  List.reverse (x :: (List.reverse list))

insertBefore : Expression -> Dimension -> Dimension -> Model -> Model
insertBefore exp insert before model =
  case exp of
    Rows ->
      { model
      | rowDimension = listInsertBefore insert before model.rowDimension
      }
    Columns ->
      { model
      | columnDimension = listInsertBefore insert before model.columnDimension
      }
    Sort ->
      { model
      | sortDimension = listInsertBefore insert before model.sortDimension
      }

listInsertBefore : a -> a -> List a -> List a
listInsertBefore insert before list =
  case list of
    head :: rest ->
      if head == before then
        insert :: head :: rest
      else
        head :: (listInsertBefore insert before rest)
    [] ->
      [insert]

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ Browser.Events.onResize (\w h -> WindowSize (w, h))
    , case model.focusWand of
      Focus _ _ -> Sub.none
      Enter _ _ -> Time.every 200 HoverTimeout
      NoFocus -> Sub.none
    , dnd.subscriptions model.draggable |> Sub.map UI
    ]


