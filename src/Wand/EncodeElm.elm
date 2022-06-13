module Wand.EncodeElm exposing (..)

import Wand exposing (Wand)

import Elm.CodeGen exposing (..)

wands : List Wand -> List Declaration
wands wds =
  [ valDecl
    Nothing
    (Just (typeVar "List Wand"))
    "wands"
    (list (List.map wand wds))
  ]

wand : Wand -> Expression
wand w =
  record
    [ ("file", w.file |> string)
    , ("gripX", w.gripX |> int)
    , ("gripY", w.gripY |> int)
    , ("tipX", w.tipX |> int)
    , ("tipY", w.tipY |> int)
    , ("castDelay", w.castDelay |> int)
    , ("actions", w.actions |> int)
    , ("shuffle", w.shuffle |> int)
    , ("slots", w.slots |> int)
    , ("spread", w.spread |> int)
    , ("reloadTime", w.reloadTime |> int)
    ]
