module ImportData exposing (..)

import Console
import LuaData
import LuaData.Parser
import LuaData.Decode
import Wand exposing(Wand)
import Wand.EncodeElm as EncodeElm

import Dict exposing (Dict)
import Elm.CodeGen as Elm
import Elm.Pretty
import Json.Decode as Decode

main : Program () Model Msg
main = Platform.worker
  { init = init
  , update = update
  , subscriptions = subscriptions
  }

type alias Model =
  { wands : List Wand
  , filesWritten : List String
  }

type Msg
  = Exit
  | ConsoleEvent (Result Decode.Error Console.Event)

type File
  = Wands (List Wand)

init : () -> (Model, Cmd Msg)
init _ =
  ( { wands = []
    , filesWritten = []
    }
  , Cmd.batch
    [ Console.write "start"
    , Console.readFile "data/scripts/gun/procedural/wands.lua"
    ]
  )

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Exit ->
      (model, Console.exit)
    ConsoleEvent (Ok (Console.ReadFile name (Ok contents))) ->
      case parseFile name contents of
        Ok (Wands wands) -> 
          ( {model | wands = wands}
          , Console.writeFile "Wand/Generated.elm" (generateFile wands)
          )
        Err err ->
          ( model
          , Cmd.batch
            [ Console.exit
            , Console.write ("Failed to parse " ++ name ++ " : " ++ err)
            ]
          )
    ConsoleEvent (Ok (Console.ReadFile name (Err err))) ->
      (model, Console.write ("Failed to read " ++ name ++ " : " ++ err))
    ConsoleEvent (Ok (Console.WriteFile name (Ok _))) ->
      let written = name :: model.filesWritten in
      ( { model | filesWritten = written }
      , if List.length written == 1 then
          Cmd.batch
            [ Console.exit
            , Console.write "done"
            ]
        else
          Cmd.none
      )
    ConsoleEvent (Ok (Console.WriteFile name (Err err))) ->
      (model, Console.write ("Failed to write " ++ name ++ " : " ++ err))
    ConsoleEvent (Err err) ->
      (model, Console.write ("event decode failed " ++ (Decode.errorToString err)))

generateFile : List Wand -> String
generateFile wands =
  Elm.file
    (Elm.normalModule
      ["Wand", "Generated"]
      []
    )
    [ Elm.importStmt
      ["Wand"]
      Nothing
      (Just Elm.exposeAll)
    ]
    (EncodeElm.wands wands)
    Nothing
    |> Elm.Pretty.pretty 80

parseFile : String -> String -> Result String File
parseFile filename contents =
  case filename |> Debug.log "filename" of
    "data/scripts/gun/procedural/wands.lua" ->
      LuaData.Decode.decodeString myWands contents
        |> Result.map Wands
        |> Result.mapError LuaData.Decode.errorToString
    _ ->
      Err "Unknown file"

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
    shuffle = LuaData.Decode.int |> LuaData.Decode.map (\v -> 1 - v)
  in
  LuaData.Decode.succeed Wand
    |> with (field "file" filename)
    |> with (field "fire_rate_wait" int)
    |> with (field "actions_per_round" int)
    |> with (field "shuffle_deck_when_empty" shuffle)
    |> with (field "deck_capacity" int)
    |> with (field "spread_degrees" int)
    |> with (field "reload_time" int)

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ Console.signal Console.SigInt Exit
    , Console.event ConsoleEvent
    ]
