module LuaDataDecodeTest exposing (..)

import LuaData.Decode exposing (..)
import LuaData exposing (..)

import Dict

import Expect exposing (Expectation)
import Test exposing (..)


suite : Test
suite =
  describe "Lua Data" 
    [ describe "values"
      [ test "strings" <| \_ ->
        decodeValue string (LuaString "string")
          |> Expect.equal (Ok "string")
      , test "ints" <| \_ ->
        decodeValue int (LuaInt 1)
          |> Expect.equal (Ok 1)
      ]
    , describe "tables"
      [ test "fields" <| \_ ->
        decodeValue (field "name" int) (LuaTable (Dict.fromList [("name", LuaInt 1)]))
          |> Expect.equal (Ok 1)
      ]
    , describe "arrays"
      [ test "array" <| \_ ->
        decodeValue (array int) (LuaArray [LuaInt 1, LuaInt 2])
          |> Expect.equal (Ok [1, 2])
      ]
    , describe "utility"
      [ test "succeed" <| \_ ->
        decodeValue (succeed 2) (LuaInt 1)
          |> Expect.equal (Ok 2)
      , test "fail" <| \_ ->
        decodeValue (fail "oopsie") (LuaInt 1)
          |> Expect.equal (Err (Failure "oopsie" (LuaInt 1)))
      , test "map" <| \_ ->
        decodeValue (map ((+) 1) int) (LuaInt 1)
          |> Expect.equal (Ok 2)
      , test "map2" <| \_ ->
        decodeValue (map2 (+) int int) (LuaInt 1)
          |> Expect.equal (Ok 2)
      ]
    , describe "pipeline"
      [ test "map" <| \_ ->
        decodeValue
          (succeed (\name -> {name = name})
            |> with (field "name" int)
          )
          (LuaTable (Dict.fromList
            [ ("name", LuaInt 1)
            ]))
          |> Expect.equal (Ok {name = 1})
      , test "table pipeline" <| \_ ->
        decodeValue
          (succeed (\name other -> {name = name, other = other})
            |> with (field "name" int)
            |> with (field "other" int)
          )
          (LuaTable (Dict.fromList
            [ ("name", LuaInt 1)
            , ("other", LuaInt 2)
            ]))
          |> Expect.equal (Ok {name = 1, other = 2})
      ]
    ]
