module LuaDataParserTest exposing (..)

import LuaData.Parser exposing (..)
import LuaData exposing (..)

import Parser.Advanced exposing (run)
import Dict

import Expect exposing (Expectation)
import Test exposing (..)


suite : Test
suite =
  describe "Lua Data" 
    [ describe "values"
      [ test "strings" <| \_ ->
        run stringValue "\"Spread staff\""
          |> Expect.equal (Ok "Spread staff")
      , test "ints" <| \_ ->
        run intValue "21"
          |> Expect.equal (Ok 21)
      ]
    , describe "tables"
      [ test "keys" <| \_ ->
        run key "name"
          |> Expect.equal (Ok "name")
      , test "key value pair" <| \_ ->
        run keyValue "grip_x = 2"
          |> Expect.equal (Ok ("grip_x", LuaInt 2))
      ]
    , describe "any value"
      [ test "value string" <| \_ ->
        run luaValue "\"Spread staff\""
          |> Expect.equal (Ok (LuaString "Spread staff"))
      , test "value int" <| \_ ->
        run luaValue "21"
          |> Expect.equal (Ok (LuaInt 21))
      , test "empty table" <| \_ ->
        run luaValue "{}"
          |> Expect.equal (Ok (LuaTable ([] |> Dict.fromList)))
      , test "value table" <| \_ ->
        run luaValue "{grip_x = 2,\ngrip_y = 5}"
          |> Expect.equal (Ok (LuaTable
            ([ ("grip_x", LuaInt 2)
            , ("grip_y", LuaInt 5)
            ] |> Dict.fromList)
          ))
      , test "value array" <| \_ ->
        run luaValue "{2, 5}"
          |> Expect.equal (Ok (LuaArray
            ([ LuaInt 2
            , LuaInt 5
            ])
          ))
      ]
    , describe "top level"
      [ test "oneline" <| \_ ->
        run keyValue "wands = {{name=\"foo\"}}"
          |> Expect.equal (Ok ("wands", LuaArray [LuaTable (Dict.fromList [("name", LuaString "foo")])]))
      , test "multiline" <| \_ ->
        run keyValue "wands = { {name=\"foo\"}}"
          |> Expect.equal (Ok ("wands", LuaArray [LuaTable (Dict.fromList [("name", LuaString "foo")])]))
      , test "wands list" <| \_ ->
        run topLevel wands
          |> isOkay
          |> Expect.equal True
      ]
    ]

isOkay : Result e v -> Bool
isOkay result =
  case result of
    Ok v ->
      --let _ = Debug.log "parsed" v in
      True
    Err e ->
      let _ = Debug.log "parse error" e in
      False

wands = """
wands =
{
  {
  name = "Spread staff",
  file = "data/items_gfx/wands/wand_0000.png",
  grip_x = 2,
  grip_y = 5,
  tip_x = 21,
  tip_y = 5,
  fire_rate_wait = 4,
  actions_per_round = 1,
  shuffle_deck_when_empty = 0,
  deck_capacity = 5,
  spread_degrees= 1,
  reload_time = 1,
  },
  {
  name = "Burst staff",
  file = "data/items_gfx/wands/wand_0001.png",
  grip_x = 2,
  grip_y = 5,
  tip_x = 19,
  tip_y = 5,
  fire_rate_wait = 3,
  actions_per_round = 0,
  shuffle_deck_when_empty = 1,
  deck_capacity = 6,
  spread_degrees= 0,
  reload_time = 1,
  },
}
"""
