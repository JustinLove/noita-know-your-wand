module NoitaKnowYourWand exposing (..)

import Log
import LuaData
import LuaData.Parser
import LuaData.Decode
import View exposing (Expression(..), DropTarget(..))
import Wand exposing (Wand, Dimension(..))

import Browser
import Dom.DragDrop as DragDrop
import Http
import Parser.Advanced as Parser
import PivotTable

type Msg
  = UI (View.Msg)
  | GotWands (Result Http.Error (List Wand))

type alias Model =
  { wands : PivotTable.Table Wand
  , rowDimension : List Dimension
  , columnDimension : List Dimension
  , sortDimension : List Dimension
  , dragDropState : DragDrop.State Dimension DropTarget
  , showingControls : Bool
  , showingHeaders : Bool
  }

main = Browser.document
  { init = init
  , view = View.document UI
  , update = update
  , subscriptions = subscriptions
  }

init : () -> (Model, Cmd Msg)
init flags =
  ( { wands = PivotTable.makeTable []
    , rowDimension = [CastDelay]
    , columnDimension = [Actions, Shuffle]
    , sortDimension = [Slots, Spread, ReloadTime]
    , dragDropState = DragDrop.initialState
    , showingControls = True
    , showingHeaders = True
    }
  , fetchWands)

update msg model =
  case msg of
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
    UI (View.ToggleControls) ->
      ( { model | showingControls = not model.showingControls }, Cmd.none )
    UI (View.ToggleHeaders) ->
      ( { model | showingHeaders = not model.showingHeaders }, Cmd.none )
    GotWands (Ok wands) ->
      ({model | wands = PivotTable.makeTable wands}, Cmd.none)
    GotWands (Err error) ->
      (model, Log.httpError "fetch error: wands" error)

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
  Sub.none

myWands : LuaData.Decode.Decoder (List Wand)
myWands =
  LuaData.Decode.array myWand

myWand : LuaData.Decode.Decoder Wand
myWand =
  let
    with = LuaData.Decode.with
    field = LuaData.Decode.field
    string = LuaData.Decode.string
    int = LuaData.Decode.int
  in
  LuaData.Decode.succeed Wand
    |> with (field "file" string)
    |> with (field "grip_x" int)
    |> with (field "grip_y" int)
    |> with (field "tip_x" int)
    |> with (field "tip_y" int)
    |> with (field "fire_rate_wait" int)
    |> with (field "actions_per_round" int)
    |> with (field "shuffle_deck_when_empty" int)
    |> with (field "deck_capacity" int)
    |> with (field "spread_degrees" int)
    |> with (field "reload_time" int)

fetchWands : Cmd Msg
fetchWands =
  Http.get
    { url = "wands.lua"
    , expect = expectLua GotWands myWands
    }


expectLua : (Result Http.Error a -> msg) -> LuaData.Decode.Decoder a -> Http.Expect msg
expectLua tagger decoder =
  Http.expectString (receiveLua decoder >> tagger)

receiveLua : LuaData.Decode.Decoder a -> Result Http.Error String -> Result Http.Error a
receiveLua decoder result =
  result
    |> Result.andThen (LuaData.Decode.decodeString decoder >> Result.mapError (LuaData.Decode.errorToString >> Http.BadBody))
