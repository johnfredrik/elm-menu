module Main exposing (..)

import Dom
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json
import Debug exposing (..)


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
    , weeks : List Week
    , recipes : List Recipe
    , recipeName : String
    , recipeId : Int
    }


model : Model
model =
    { recipeId = 0
    , title = ""
    , weeks = [ (initWeek 0), (initWeek 1), (initWeek 2) ]
    , recipes = []
    , recipeName = ""
    }


type alias Recipe =
    { id : Int
    , name : String
    , editing : Bool
    }


type alias Day =
    { id : Int, recipe : Recipe, editing : Bool }


type alias Week =
    { number : Int
    , days : List Day
    }


initWeek : Int -> Week
initWeek number =
    { number = number
    , days = [ { id = 0, recipe = initRecipe, editing = False }
             , { id = 1, recipe = initRecipe, editing = False }
             , { id = 2, recipe = initRecipe, editing = False }
             , { id = 3, recipe = initRecipe, editing = False }
             , { id = 4, recipe = initRecipe, editing = False }
             , { id = 5, recipe = initRecipe, editing = False }
             , { id = 6, recipe = initRecipe, editing = False }
    ]
    }


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
    | EditDayRecipe Int Int Bool
    | UpdateDayRecipe Int Day String



update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model ! []

        UpdateRecipeName name ->
            { model | recipeName = name } ! []

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
                { model | recipes = List.map updateRecipe model.recipes } ! []

        EditDayRecipe weekNumber dayId isEditing ->
            let
              debug = log "weeksNumber" weekNumber
              updateDay day =
                if (day.id == dayId) then
                    { day | editing = isEditing }
                else
                    day

              updateWeek week =
                if (week.number == weekNumber) then
                    {week | days = List.map updateDay week.days }
                else
                    week
            in
              { model | weeks = List.map updateWeek model.weeks } ! []
        
        UpdateDayRecipe weekNumber day recipeId ->
            let
                
                id = Result.withDefault 0 (String.toInt recipeId)
                recipe = getRecipe id
                debug = log "recipe" recipe
                updateDay d = 
                    if (d.id == day.id) then 
                        {d | recipe = recipe }
                    else
                        d
             
                updateWeek week =
                        if (week.number == weekNumber) then
                            {week | days = List.map updateDay week.days}
                        else
                            week
            in
              model ! []

updateWeek : List Week -> Week -> List Week
updateWeek weeks week =
    let
        filteredWeeks =
            List.filter (\n -> n.number == week.number) weeks
    in
        List.sortBy .number (week :: filteredWeeks)


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

-- VIEW
-- Html is defined as: elem [ attribs ][ children ]
-- CSS can be applied via class names or inline style attrib


view : Model -> Html Msg
view model =
    div [ class "container", style [ ( "margin-top", "30px" ), ( "text-align", "center" ) ] ]
        [ -- inline CSS (literal)
          div [ class "row" ]
            [ div [ class "col-md-2" ]
                [ h5 [] [ text "Recipes:" ]
                , input
                    [ placeholder "Add new recipe"
                    , value model.recipeName
                    , onInput UpdateRecipeName
                    , onEnter AddRecipe
                    ]
                    []
                , recipeList model.recipes
                ]
            , div [ class "col-md-10" ] [ calender model ]
            ]
        ]


calenderHeader : Html Msg
calenderHeader =
    div [ class "col-md-12" ]
        [ h5 [ class "col-md-1" ] [ text "Week" ]
        , h5 [ class "col-md-1" ] [ text "Mon" ]
        , h5 [ class "col-md-1" ] [ text "Tue" ]
        , h5 [ class "col-md-1" ] [ text "Wed" ]
        , h5 [ class "col-md-1" ] [ text "Thu" ]
        , h5 [ class "col-md-1" ] [ text "Fri" ]
        , h5 [ class "col-md-1" ] [ text "Sat" ]
        , h5 [ class "col-md-1" ] [ text "Sun" ]
        ]


calender : Model -> Html Msg
calender model =
    div []
        [ calenderHeader
        , viewWeeks model
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
            , onEnter (EditRecipe recipe.id False)
            ]
            []
        ]


recipeList : List Recipe -> Html Msg
recipeList recipes =
    ul [ class "recipe-list" ] (List.map recipeSingle recipes)


viewWeeks : Model -> Html Msg
viewWeeks model =
    div [] (List.map (viewWeek model) model.weeks)


viewWeek : Model -> Week -> Html Msg
viewWeek model week =
    div [ class "col-md-12 week" ]
        [ div [ class "col-md-1 week-number" ] [ text (toString week.number) ]
        , span [] (List.map (viewDay week) week.days)
        ]


viewDay : Week -> Day -> Html Msg
viewDay week day =
    div
        [ classList [ ( "col-md-1", True ), ( "editing", day.editing ) ] ]
        [ div
            [ class "recipe-box view", onDoubleClick (EditDayRecipe week.number day.id True) ]
            [ label
                [ ]
                [ text day.recipe.name ]
            ]
        , input
            [ class "edit"
            , value day.recipe.name
            , onEnter (EditDayRecipe week.number day.id False)
            , onInput (UpdateDayRecipe week.number day)
            ]
            []
        ]
