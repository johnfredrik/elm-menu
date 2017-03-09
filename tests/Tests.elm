module Tests exposing (..)
import Debug exposing (..)
import Test exposing (..)
import Expect
import Fuzz exposing (list, int, tuple, string)
import Menu exposing (..)


all : Test
all =
    describe "Recipe Menu test"
        [ describe "Update tests"
            [ test "it has no side effects" <|
                \() ->
                    model
                        |> update NoOp
                        |> Tuple.second 
                        |> Expect.equal Cmd.none
            ]
        , describe "Recipe results"
            [ fuzz (list int) "recipes exists" <|
                \recipeList ->
                    List.length Menu.recipes |> Expect.atLeast 0
            , test "recipe results return atleased one" <|
                \() ->
                    Expect.equal 1 (List.length (Menu.searchRecipe "Pas"))
            , test "recipe results return atleased one case insensitive" <|
                \() ->
                    Expect.equal 1 (List.length (Menu.searchRecipe "pas"))
            , fuzz(list int) "UpdateDayRecipe adds results on day" <|
               \resultList ->
                    let 
                        tuple = Menu.update (UpdateDayRecipe 1 "pas") Menu.model
                        model = Tuple.first tuple
                        day = Maybe.withDefault (Menu.initDay 1) (Menu.find (\n -> n.id == 1) model.week.days)
                    in
                        List.length day.recipeResults |> Expect.atLeast 1
            
            , fuzz(list int) "UpdateDayRecipe does not add recipe result on day with key less than 3 chars" <|
                \resultList ->
                    let 
                        tuple = Menu.update (UpdateDayRecipe 1 "pa") Menu.model
                        model = Tuple.first tuple
                        day = Maybe.withDefault (Menu.initDay 1) (Menu.find (\n -> n.id == 1) model.week.days)
                    in
                        List.length day.recipeResults |> Expect.equal 0
            ]
        ]
