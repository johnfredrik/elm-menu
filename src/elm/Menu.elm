port module Menu exposing (..)

import Dom exposing (focus)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json
import Debug exposing (..)
import Task
import Regex


-- APP


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


init : ( Model, Cmd Msg )
init =
    model ! []



-- MODEL


type alias Model =
    { title : String
    , week : Week
    , recipes : List Recipe
    , recipeName : String
    , recipeId : Int
    , dayRecipeName : String
    }


model : Model
model =
    { recipeId = 0
    , title = ""
    , week = initWeek
    , recipes = []
    , recipeName = ""
    , dayRecipeName = ""
    }


type alias Recipe =
    { id : Int
    , name : String
    , editing : Bool
    }


type alias Day =
    { id : Int, recipe : Recipe, editing : Bool }


type alias Week =
    {days : List Day}


initWeek :  Week
initWeek =
    {  days = 
        [ initDay 1
        , initDay 2
        , initDay 3
        , initDay 4
        , initDay 5
        , initDay 6
        , initDay 7
    ]
    }

initDay : Int -> Day
initDay id =
    let
      debug = log "ID" id
    in
        { id = id, recipe = initRecipe, editing = False }

initRecipe : Recipe
initRecipe =
    { id = -1, name = "", editing = False }


newRecipe : Int -> String -> Recipe
newRecipe id name =
    { id = id, name = name, editing = False }



-- UPDATE


type Msg
    = NoOp
    | AddRecipe
    | UpdateRecipe Int String
    | EditRecipe Int Bool
    | UpdateRecipeName String
    | EditDayRecipe Int Bool
    | UpdateDayRecipeName String
    | UpdateDayRecipe Int String



update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model ! []

        UpdateRecipeName name ->
            { model | recipeName = name } ! []
        
        UpdateDayRecipeName name ->
            { model | dayRecipeName = name } ! []
        AddRecipe ->
            { model
                | recipeId = model.recipeId + 1
                , recipeName = ""
                , recipes =
                    if String.isEmpty model.recipeName then
                        model.recipes
                    else
                        model.recipes ++ [ newRecipe model.recipeId model.recipeName ]
            }
                ! []

        UpdateRecipe id name ->
            let
                updateRecipe recipe =
                    if recipe.id == id then
                        { recipe | name = name }
                    else
                        recipe
            in
                { model | recipes = List.map updateRecipe model.recipes } ! []

        EditRecipe id isEditing ->
            let
                updateRecipe recipe =
                    if recipe.id == id then
                        { recipe | editing = isEditing }
                    else
                        recipe

                focus =
                    Dom.focus ("recipe-" ++ toString id)
            in
                { model | recipes = List.map updateRecipe model.recipes } 
                    ! [ Task.attempt (\_ -> NoOp) focus]

        EditDayRecipe dayId isEditing ->
            let
              updateDay day =
                if (day.id == dayId) then
                    { day | editing = isEditing }
                else
                    day
              updateWeek week =
                {week | days = List.map updateDay week.days}

              focus =
                    Dom.focus ("day-recipe-" ++ toString dayId)
              debug = log "focus" focus
            in
              { model | week = updateWeek model.week } 
                ! [ Task.attempt (\_ -> NoOp) focus]
        
        UpdateDayRecipe dayId name ->
            let
                d1 = log "name" name
                oldWeek = model.week
                newWeek = {oldWeek | days = List.map updateDay oldWeek.days}
                oldDay = Maybe.withDefault (initDay dayId) (find (\n -> n.id == dayId) model.week.days)
                oldRecipe =  oldDay.recipe
                newRecipe = 
                    { oldRecipe | name = name}
                
                updateDay d = 
                    if (d.id == dayId) then 
                        {d | recipe = newRecipe }
                    else
                        d
            in
              {model | week = newWeek} ! []

recipes : List Recipe
recipes =
    [ { id = 1, name = "Pasta", editing = False }
    , { id = 2, name = "Taco", editing = False }
    , { id = 3, name = "Lasagne", editing = False }
    , { id = 4, name = "Tandoori", editing = False }
    ]


numberOfWeeks : Int
numberOfWeeks =
    4


onEnter : Msg -> Attribute Msg
onEnter msg =
    let
        isEnter code =
            if code == 13 then
                Json.succeed msg
            else
                Json.fail "not ENTER"
    in
        on "keydown" (Json.andThen isEnter keyCode)


find : (a -> Bool) -> List a -> Maybe a
find predicate list =
    case list of
        [] ->
            Nothing

        first :: rest ->
            if predicate first then
                Just first
            else
                find predicate rest

getRecipe : Int -> Recipe
getRecipe recipeId =
    let
      recipe = find (\n -> n.id == recipeId) recipes
      debug = log "recipe" recipe
    in
      Maybe.withDefault initRecipe recipe

searchRecipe : String -> List Recipe
searchRecipe key =
    let
        pattern = Regex.caseInsensitive (Regex.regex ("^" ++ key))
        startsWith recipe = 
            Regex.contains pattern recipe.name
    in
      List.filter startsWith recipes


-- VIEW
-- Html is defined as: elem [ attribs ][ children ]
-- CSS can be applied via class names or inline style attrib


view : Model -> Html Msg
view model =
    div [ class "container", style [ ( "margin-top", "30px" ), ( "text-align", "center" ) ] ]
        [ -- inline CSS (literal)
          div [ class "row" ]
            [ --div [ class "col-lg-2" ]
            --     [ h5 [] [ text "Recipes:" ]
            --     , input
            --         [ placeholder "Add new recipe"
            --         , value model.recipeName
            --         , onInput UpdateRecipeName
            --         , onEnter AddRecipe
            --         ]
            --         []
            --     , recipeList model.recipes
            --     ]
            div [] [ calender model ]
            -- , div []
            --     [ input [] []

            --     ]
             ]
        ]


calenderHeader : Html Msg
calenderHeader =
    div []
        [ h5 [ class "day" ] [ text "Mon" ]
        , h5 [ class "day" ] [ text "Tue" ]
        , h5 [ class "day" ] [ text "Wed" ]
        , h5 [ class "day" ] [ text "Thu" ]
        , h5 [ class "day" ] [ text "Fri" ]
        , h5 [ class "day" ] [ text "Sat" ]
        , h5 [ class "day" ] [ text "Sun" ]
        ]


calender : Model -> Html Msg
calender model =
    div []
        [ calenderHeader
        , viewWeek model.week
        ]


recipeOption : Recipe -> Html Msg
recipeOption recipe =
    option [ Html.Attributes.value (toString recipe.id) ] [ text (recipe.name) ]


recipeSingle : Recipe -> Html Msg
recipeSingle recipe =
    li [ classList [ ( "editing", recipe.editing) ]]
        [ div
            [ class "view" ]
            [ label
                [ onDoubleClick (EditRecipe recipe.id True) ]
                [ text recipe.name ]
            ]
        , input
            [ class "edit"
            , value recipe.name
            , id ("recipe-" ++ toString recipe.id)
            , onInput (UpdateRecipe recipe.id)
            , onBlur (EditRecipe recipe.id False)
            , onEnter (EditRecipe recipe.id False)
            ]
            []
        ]


recipeList : List Recipe -> Html Msg
recipeList recipes =
    ul [ class "recipe-list" ] (List.map recipeSingle recipes)

viewWeek : Week -> Html Msg
viewWeek week =
    div [ class "week" ]
        [ span [] (List.map (viewDay week) week.days)
        ]


viewDay : Week -> Day -> Html Msg
viewDay week day =
    div
        [ classList [( "day", True ), ( "editing", day.editing ) ] ]
        [ div
            [ class "recipe view no-padding", onDoubleClick (EditDayRecipe day.id True) ]
            [ label
                [ ]
                [ text day.recipe.name ]
            ]
        , div
            [ class "recipe"]
            [ input
                [ class "edit"
                , id ("day-recipe-" ++ toString day.id)
                , placeholder "Recipe name"
                , autofocus True
                , value day.recipe.name
                , onEnter (EditDayRecipe day.id False)
                , onBlur (EditDayRecipe day.id False)
                , onInput (UpdateDayRecipe day.id)
                ]
                []
            ]
        ]

-- viewRecipeResults : String -> Html Msg
-- viewRecipeResults key = 
--     let
     
--     in
--       recipeList result
