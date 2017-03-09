port module Menu exposing (..)

import Dom exposing (focus)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json
import Debug exposing (..)
import Task
import Regex
import Autocomplete


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
    , recipeResults : List Recipe
    , selectedResultRecipe : Int
    , autoState: Autocomplete.State
    }


model : Model
model =
    { recipeId = 0
    , title = ""
    , week = initWeek
    , recipes = []
    , recipeName = ""
    , recipeResults = []
    , selectedResultRecipe = -1
    , autoState = Autocomplete.empty
    }


type alias Recipe =
    { id : Int
    , name : String
    , editing : Bool
    , selected : Bool
    }


type alias Day =
    { id : Int
    , recipe : Recipe
    , editing : Bool
    }


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
    { id = id, recipe = initRecipe, editing = False}

initRecipe : Recipe
initRecipe =
    { id = -1, name = "", editing = False, selected = False }


newRecipe : Int -> String -> Recipe
newRecipe id name =
    { id = id, name = name, editing = False, selected = False }



-- UPDATE


type Msg
    = NoOp
    | AddRecipe
    | UpdateRecipe Int String
    | EditRecipe Int Bool
    | UpdateRecipeName String
    | EditDayRecipe Int Bool
    | UpdateDayRecipe Int String
    | SelectDayRecipe Int String
    | SelectResultRecipe



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
                { model | recipes = List.map updateRecipe model.recipes } 
                    ! [ Task.attempt (\_ -> NoOp) focus]

        EditDayRecipe dayId isEditing ->
            let
              focus =
                    Dom.focus ("day-recipe-" ++ toString dayId)
            in
              { model | week = (editDayRecipeInWeek model.week dayId isEditing), recipeResults = []} 
                ! [ Task.attempt (\_ -> NoOp) focus]
        
        UpdateDayRecipe dayId name ->
            let
                recipeResults = searchRecipe name
            in
                {model | week = (updateDayRecipeInWeek model.week dayId name), recipeResults = recipeResults} ! []

        SelectDayRecipe dayId name ->
            let 
                week = (updateDayRecipeInWeek model.week dayId name)
            in
                {model | week = (editDayRecipeInWeek week dayId False)} ! []
        SelectResultRecipe ->
            let
                next = getNextResultRecipe model.selectedResultRecipe model.recipeResults
                debug = log "Test" next.id
                updateRecipe recipe =
                    if (recipe.id == next.id) then
                        {recipe | selected = True}
                    else 
                        {recipe | selected = False}
                        
            in
              {model | recipeResults = List.map updateRecipe model.recipeResults, selectedResultRecipe = next.id} ! []

getNextResultRecipe : Int -> List Recipe -> Recipe
getNextResultRecipe previousId resultRecipes =
    let
        next = Maybe.withDefault initRecipe (nextTo (\n -> n.id == previousId) resultRecipes)
        debug = log "getNextResultRecipe" next
    in
        if (next.id == -1 && List.length resultRecipes > 0) then
            Maybe.withDefault initRecipe (List.head resultRecipes)
        else
            next

nextTo : (a -> Bool) -> List a -> Maybe a
nextTo predicate list =
    case list of
        [] ->
            Nothing
        first :: rest ->
            if predicate first then
                List.head rest
            else 
                find predicate rest

editDayRecipeInWeek : Week -> Int -> Bool -> Week
editDayRecipeInWeek week dayId isEditing =
    let
        updateDay day =
        if (day.id == dayId) then
            { day | editing = isEditing }
        else
            { day | editing = False }
        updateWeek week =
        {week | days = List.map updateDay week.days}

        focus =
            Dom.focus ("day-recipe-" ++ toString dayId)
    in
        updateWeek week
        

updateDayRecipeInWeek : Week -> Int -> String -> Week
updateDayRecipeInWeek week dayId name =
    let
        newWeek = {week | days = List.map updateDay week.days}
        oldDay = Maybe.withDefault (initDay dayId) (find (\n -> n.id == dayId) model.week.days)
        oldRecipe =  oldDay.recipe
        newRecipe = 
            { oldRecipe | name = name}
        
        updateDay d = 
            if (d.id == dayId) then 
                {d | recipe = newRecipe}
            else
                d
    in
        newWeek

recipes : List Recipe
recipes =
    [ { id = 1, name = "Pasta", editing = False, selected = False }
    , { id = 2, name = "Taco", editing = False, selected = False }
    , { id = 3, name = "Lasagne", editing = False, selected = False }
    , { id = 4, name = "Tandoori", editing = False, selected = False }
    , { id = 5, name = "Pizza White", editing = False, selected = False }
    , { id = 6, name = "Pizza Red", editing = False, selected = False }
    ]

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

onArrowDown : Msg -> Attribute Msg
onArrowDown msg =
    let 
        
        isArrowDown code =
            if code == 40 then
                Json.succeed msg
            else 
                Json.fail "not Arrow down key"
    in 
        on "keydown" (Json.andThen isArrowDown keyCode)


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
    in
      Maybe.withDefault initRecipe recipe

searchRecipe : String -> List Recipe
searchRecipe key =
    let
        pattern = Regex.caseInsensitive (Regex.regex ("^" ++ key))
        startsWith recipe = 
            Regex.contains pattern recipe.name
    in
        if (String.length key) > 2 then 
            List.filter startsWith recipes
        else []


-- VIEW
-- Html is defined as: elem [ attribs ][ children ]
-- CSS can be applied via class names or inline style attrib


view : Model -> Html Msg
view model =
    div [ class "container", style [ ( "margin-top", "30px" ), ( "text-align", "center" ) ] ]
        [ -- inline CSS (literal)
          div [ class "row" ]
            [ div [] [ calender model ] ]
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
        , viewWeek model
        ]


recipeOption : Recipe -> Html Msg
recipeOption recipe =
    option [ Html.Attributes.value (toString recipe.id) ] [ text (recipe.name) ]


recipeSingle : Recipe -> Html Msg
recipeSingle recipe =
    li [ classList [ ("recipe", True), ("editing", recipe.editing) ]]
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
            --, onBlur (EditRecipe recipe.id False)
            , onEnter (EditRecipe recipe.id False)
            ]
            []
        ]


recipeList : List Recipe -> Html Msg
recipeList recipes =
    ul [ class "recipe-list" ] (List.map recipeSingle recipes)

viewWeek : Model -> Html Msg
viewWeek model =
    div [ class "week" ]
        [ span [] (List.map (viewDay model) model.week.days)
        ]


viewDay : Model -> Day -> Html Msg
viewDay model day =
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
                --, onBlur (EditDayRecipe day.id False)
                , onInput (UpdateDayRecipe day.id)
                , onArrowDown SelectResultRecipe
                ]
                []
            ]
        , div 
            [ class "recipe-results" ]
            [ viewRecipeResults day.id model.recipeResults ]
        ]

viewRecipeResults : Int -> List Recipe -> Html Msg
viewRecipeResults dayId recipes = 
    ul [ class "edit"] (List.map (viewSingleRecipeResult dayId) recipes)

viewSingleRecipeResult : Int -> Recipe -> Html Msg
viewSingleRecipeResult dayId recipe =
    li  [ classList [("selected", recipe.selected)]
        , onClick (SelectDayRecipe dayId recipe.name)
        ]
        [ div
            []
            [ text recipe.name ]
        ]
