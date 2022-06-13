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
  , slots : Int
  , spread : Int
  , reloadTime : Int
  }

type Dimension
  = CastDelay
  | Actions
  | Shuffle
  | Slots
  | Spread
  | ReloadTime

attribute : Dimension -> (Wand -> Int)
attribute dim =
  case dim of
    CastDelay -> .castDelay
    Actions -> .actions
    Shuffle -> .shuffle
    Slots -> .slots
    Spread -> .spread
    ReloadTime -> .reloadTime

values : Dimension -> List String
values dim =
  case dim of
    CastDelay ->
      [ "0.03"
      , "0.15"
      , "0.27"
      , "0.38"
      , "0.50"
      ]
    Actions ->
      [ "1"
      , "2"
      , "3+"
      ]
    Shuffle ->
      [ "Non"
      , "Sfl"
      ]
    Slots ->
      [ "01-04"
      , "05-07"
      , "08-10"
      , "11-13"
      , "14-16"
      , "17-19"
      , "20-22"
      , "23+"
      ]
    Spread ->
      [ "<5"
      , "5-7"
      , ">7"
      ]
    ReloadTime ->
      [ "<0.5"
      , "0.5-1"
      , ">1"
      ]

name : Dimension -> String
name dim =
  case dim of
    CastDelay -> "Cast Delay"
    Actions -> "Actions"
    Shuffle -> "Shuffle"
    Slots -> "Slots"
    Spread -> "Spread"
    ReloadTime -> "Reload Time"
