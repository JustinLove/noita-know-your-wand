module Wand.Live exposing (..)

import LuaData
import LuaData.Parser
import LuaData.Decode
import Wand exposing (Wand, Dimension(..))

import Http
import Parser.Advanced as Parser

myWands : LuaData.Decode.Decoder (List Wand)
myWands =
  LuaData.Decode.array myWand

myWand : LuaData.Decode.Decoder Wand
myWand =
  let
    with = LuaData.Decode.with
    field = LuaData.Decode.field
    string = LuaData.Decode.string
    filename = string |> LuaData.Decode.map (\file -> String.replace "data/items_gfx/wands/" "" file)
    int = LuaData.Decode.int
  in
  LuaData.Decode.succeed Wand
    |> with (field "file" filename)
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

fetchWands : (Result Http.Error (List Wand) -> msg) -> Cmd msg
fetchWands tagger =
  Http.get
    { url = "data/scripts/gun/procedural/wands.lua"
    , expect = expectLua tagger myWands
    }

expectLua : (Result Http.Error a -> msg) -> LuaData.Decode.Decoder a -> Http.Expect msg
expectLua tagger decoder =
  Http.expectString (receiveLua decoder >> tagger)

receiveLua : LuaData.Decode.Decoder a -> Result Http.Error String -> Result Http.Error a
receiveLua decoder result =
  result
    |> Result.andThen (LuaData.Decode.decodeString decoder >> Result.mapError (LuaData.Decode.errorToString >> Http.BadBody))
