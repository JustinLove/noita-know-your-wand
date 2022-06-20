module ComparableTest exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)


suite : Test
suite =
  describe "Comparable tuples" 
    [ test "by which part" <| \_ ->
      ((1,2) < (2,1))
        |> Expect.equal True
    , test "reversed" <| \_ ->
      ((2,1) < (1,2))
        |> Expect.equal False
    , test "string" <| \_ ->
      ("a" < "b")
        |> Expect.equal True
    , test "multiple types" <| \_ ->
      ((1,"b") < (2,"a"))
        |> Expect.equal True
    ]
