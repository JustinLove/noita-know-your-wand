module LuaData.Parser exposing (..)

import Parser.Advanced exposing (..)

import Set
import Dict exposing (Dict)

type alias Context = String
type alias Problem = String
type alias LuaDataParser a = Parser Context Problem a

type LuaValue
  = LuaString String
  | LuaInt Int
  | LuaTable LuaDict
  | LuaArray (List LuaValue)

type alias LuaDict = Dict String LuaValue

topLevel : LuaDataParser (String, LuaValue)
topLevel =
  succeed identity
    |. spaces
    |= keyValue
    |. spaces
    |. end "not at end of input"
    |> inContext "top level"

luaValue : LuaDataParser LuaValue
luaValue =
  oneOf
    [ succeed LuaString
      |= stringValue
    , succeed LuaInt
      |= intValue
    , lazy (\_ -> tableOrArray)
    ]
    |> inContext "lua value"

stringValue : LuaDataParser String
stringValue =
  succeed identity
    |. symbol (Token "\"" "looking for an open \"")
    |= getChompedString (chompWhile (\c -> c /= '"'))
    |. symbol (Token "\"" "looking for a close \"")

intValue = int "expecting int" "invalid number"

key : LuaDataParser String
key =
  variable
    { start = \c -> Char.isAlpha c || c == '_'
    , inner = \c -> Char.isAlphaNum c || c == '_'
    , reserved = Set.fromList luaReservedWords
    , expecting = "Expecting a table key"
    }

keyValue : LuaDataParser (String, LuaValue)
keyValue =
  succeed (Tuple.pair)
    |= key
    |. spaces
    |. symbol (Token "=" "Expecting '=' in key value pair")
    |. spaces
    |= luaValue
    |> inContext "key and value"

tableOrArray : LuaDataParser LuaValue
tableOrArray =
  succeed identity
    |. token (Token "{" "Expecting '{' to start table")
    |. spaces
    |= oneOf
      [ succeed (Dict.fromList >> LuaTable)
        |= tableEntries keyValue
      , succeed LuaArray
        |= tableEntries luaValue
      ]
    |> inContext "table or array"

tableEnd = token (Token "}" "Expecting '}' to close table")

tableEntries : LuaDataParser a -> LuaDataParser (List a)
tableEntries parseItem =
  oneOf
    [ parseItem |> andThen (\item -> loop [item] (tableEntriesInner parseItem))
    , tableEnd |> map (\_ -> [])
    ]

tableEntriesInner : LuaDataParser a -> List a -> LuaDataParser (Step (List a) (List a))
tableEntriesInner parseItem revItems =
  let
    parseEnd = map (\_ -> Done (List.reverse revItems)) tableEnd
  in
    succeed identity
      |. spaces
      |= oneOf
        [ succeed identity
          |. token (Token "," "Expecting ',' between table entries")
          |. spaces
          |= oneOf
            [ parseItem |> map (\item -> Loop (item :: revItems))
            , parseEnd
            ]
        , parseEnd
        ]

luaReservedWords =
  [ "and"
  , "break"
  , "do"
  , "else"
  , "elseif"
  , "end"
  , "false"
  , "for"
  , "function"
  , "if"
  , "in"
  , "local"
  , "nil"
  , "not"
  , "or"
  , "repeat"
  , "return"
  , "then"
  , "true"
  , "until"
  , "while"
  ]
