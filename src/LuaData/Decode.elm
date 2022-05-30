module LuaData.Decode exposing (..)

import LuaData exposing (..)
import LuaData.Parser as Parser

import Dict
import Parser.Advanced

type Error
  = ExpectString LuaValue
  | ExpectInt LuaValue
  | ExpectTable LuaValue
  | ExpectArray LuaValue
  | Field String LuaDict
  | Failure String LuaValue
  | ParseError (List (Parser.Advanced.DeadEnd Parser.Context Parser.Problem))

type alias Decoder a = LuaValue -> Result Error a

errorToString : Error -> String
errorToString err =
  case err of
    ExpectString value ->
      "Expected a string"
    ExpectInt value ->
      "Expected an int"
    ExpectTable value ->
      "Expected table"
    ExpectArray value ->
      "Expected array"
    Field key value ->
      "Expected the table key " ++ key
    Failure message value ->
      "Decode failure: " ++ message
    ParseError parseError ->
      Parser.deadEndsToString parseError

string : Decoder String
string value =
  case value of
    LuaString s -> Ok s
    _ -> Err (ExpectString value)

int : Decoder Int
int value =
  case value of
    LuaInt i -> Ok i
    _ -> Err (ExpectInt value)

field : String -> Decoder a -> Decoder a
field key decoder value =
  case value of
    LuaTable t ->
      case Dict.get key t of
        Just v -> decoder v
        Nothing -> Err (Field key t)
    _ -> Err (ExpectTable value)

array : Decoder a -> Decoder (List a)
array decoder value =
  case value of
    LuaArray a ->
      List.foldr (\element accum ->
          Result.map2 (::) (decoder element) accum
        ) (Ok []) a
    _ -> Err (ExpectArray value)

map : (a -> result) -> Decoder a -> Decoder result
map f decoder value =
  Result.map f (decoder value)

map2 : (a -> b -> result) -> Decoder a -> Decoder b -> Decoder result
map2 f da db value =
  Result.map2 f (da value) (db value)

with : Decoder a -> Decoder (a -> result) -> Decoder result
with da db value =
  Result.map2 (|>) (da value) (db value)

succeed : a -> Decoder a
succeed result value =
  Ok result

fail : String -> Decoder a
fail err value =
  Err (Failure err value)

decodeValue : (Decoder a) -> LuaValue -> Result Error a
decodeValue decoder value =
  decoder value

decodeString : Decoder a -> String -> Result Error a
decodeString decoder s =
  Parser.Advanced.run Parser.topLevel s
    |> Result.mapError ParseError
    |> Result.andThen (\(name, value) -> decoder value)
