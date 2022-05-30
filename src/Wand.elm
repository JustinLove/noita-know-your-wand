module Wand exposing (..)

type alias Wand =
  { file : String
  , gripX : Int
  , gripY : Int
  , tipX : Int
  , tipY : Int
  , castDelay : Int
  , actions : Int
  , shuffle : Int
  , deckCapacity : Int
  , spread : Int
  , reloadTime : Int
  }

type Dimension
  = CastDelay
  | Actions
  | Shuffle
  | DeckCapacity
  | Spread
  | ReloadTime

attribute : Dimension -> (Wand -> Int)
attribute dim =
  case dim of
    CastDelay -> .castDelay
    Actions -> .actions
    Shuffle -> .shuffle
    DeckCapacity -> .deckCapacity
    Spread -> .spread
    ReloadTime -> .reloadTime

values : Dimension -> List String
values dim =
  case dim of
    CastDelay ->
      [ "0"
      , "1"
      , "2"
      ]
    Actions ->
      [ "0"
      , "1"
      , "2+"
      ]
    Shuffle ->
      [ "No"
      , "Yes"
      ]
    DeckCapacity ->
      [ "0"
      , "1"
      , "2"
      , "3"
      , "4"
      , "5"
      , "6"
      , "7"
      ]
    Spread ->
      [ "0"
      , "1"
      , "2"
      ]
    ReloadTime ->
      [ "0"
      , "1"
      , "2"
      ]
