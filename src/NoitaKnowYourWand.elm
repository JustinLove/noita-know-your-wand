module NoitaKnowYourWand exposing (..)

import View
import Wand exposing (Wand, Dimension(..))

import Log
import LuaData
import LuaData.Parser
import LuaData.Decode

import Browser
import Http
import Parser.Advanced as Parser

type Msg
  = UI (View.Msg)
  | GotWands (Result Http.Error (List Wand))

type alias Model =
  { wands : List Wand
  , rowDimension : Dimension
  , columnDimension : Dimension
  }

main = Browser.document
  { init = init
  , view = View.document UI
  , update = update
  , subscriptions = subscriptions
  }

init : () -> (Model, Cmd Msg)
init flags =
  ( { wands = []
    , rowDimension = CastDelay
    , columnDimension = Slots
    }
  , fetchWands)

update msg model =
  case msg of
    UI (View.None) ->
      (model, Cmd.none)
    UI (View.ChangedRows dim) ->
      ({model | rowDimension = dim}, Cmd.none)
    UI (View.ChangedColumns dim) ->
      ({model | columnDimension = dim}, Cmd.none)
    GotWands (Ok wands) ->
      ({model | wands = wands}, Cmd.none)
    GotWands (Err error) ->
      (model, Log.httpError "fetch error: wands" error)

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
