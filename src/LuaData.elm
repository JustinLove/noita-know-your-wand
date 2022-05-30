module LuaData exposing (..)

import Dict exposing (Dict)

type LuaValue
  = LuaString String
  | LuaInt Int
  | LuaTable LuaDict
  | LuaArray (List LuaValue)

type alias LuaDict = Dict String LuaValue

