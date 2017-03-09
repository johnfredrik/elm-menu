module Tests exposing (..)

import Test exposing (..)
import Expect
import Fuzz exposing (list, int, tuple, string)
import Menu exposing (..)


all : Test
all =
    describe "Recipe Menu test"
        [ describe "Menu"
            [ fuzz (list int) "recipes exists" <|
                \recipeList ->
                    List.length Menu.recipes |> Expect.atLeast 0
            , test "recipe results return atleased one" <|
                \() ->
                    Expect.equal 1 (List.length (Menu.searchRecipe "Pas"))
            , test "recipe results return atleased one case insensitive" <|
                \() ->
                    Expect.equal 1 (List.length (Menu.searchRecipe "pas"))
            ]
        ]
