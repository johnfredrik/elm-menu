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
        , subscriptions = subscriptions
        }

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map SetAutoState Autocomplete.subscription

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
    , selectedRecipe : Maybe Recipe
    , autoState: Autocomplete.State
    , showHowMany : Int
    , query: String
    , showMenu: Bool
    }


model : Model
model =
    { recipeId = 0
    , title = ""
    , week = initWeek
    , recipes = []
    , recipeName = ""
    , recipeResults = []
    , autoState = Autocomplete.empty
    , selectedRecipe = Nothing
    , showHowMany = 5
    , query = ""
    , showMenu = False
    }


type alias Recipe =
    { name : String
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
    { name = "", editing = False, selected = False }


newRecipe : Int -> String -> Recipe
newRecipe id name =
    { name = name, editing = False, selected = False }



-- UPDATE


type Msg
    = NoOp
    | UpdateRecipeName String
    | EditDayRecipe Int Bool
    | UpdateDayRecipe Int
-- AUTOCOMPLETE
    | SelectRecipeKeyboard String
    | SelectRecipeMouse String
    | SetQuery String
    | SetAutoState Autocomplete.Msg
    | PreviewRecipe String
    | Wrap Bool
    | Reset
    | HandleEscape
    | OnFocus
    | EditNextDayRecipe Int Int



update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model ! []

        UpdateRecipeName name ->
            { model | recipeName = name } ! []
        EditDayRecipe dayId isEditing ->
            let
              focus =
                    Dom.focus ("recipe-day-input-" ++ toString dayId)
              newWeek = (editDayRecipeInWeek model.week dayId isEditing) 
              newWeek2 =  
                case model.selectedRecipe of
                    Nothing ->
                         newWeek
                    Just recipe ->
                        (updateDayRecipeInWeek newWeek dayId recipe)
              newModel =
                {model | week = newWeek2} 
                    |> resetInput
                    |> removeSelection
            in
                newModel ! [ Task.attempt (\_ -> NoOp) focus]
        EditNextDayRecipe dayId nextDayId ->
            let
                debug = log "focus" ("recipe-day-input-" ++ toString nextDayId)
                selectedWeek =  
                  case model.selectedRecipe of
                      Nothing ->
                           model.week
                      Just recipe ->
                          (updateDayRecipeInWeek model.week dayId recipe)
                d1 = log "selectedWeek" selectedWeek
                newWeek = (editDayRecipeInWeek selectedWeek nextDayId True)
                newModel =
                  {model | week = newWeek} 
                      |> resetInput
                      |> removeSelection

                focus =
                      Dom.focus ("recipe-day-input-" ++ (toString nextDayId)) 
            in
                newModel ! [ Task.attempt (\_ -> NoOp) focus]
        UpdateDayRecipe dayId ->
            let
              debug = log "UpdateDayRecipe" toString dayId
            in
                case model.selectedRecipe of
                    Nothing ->
                        model ! []
                    Just recipe ->
                        {model | week = (updateDayRecipeInWeek model.week dayId recipe)} ! []

        SelectRecipeKeyboard id ->
            let
                debug = log "SelectRecipeKeyboard" id
                newModel =
                    setQuery model id
                        |> resetMenu
            in
                newModel ! []
        SelectRecipeMouse id ->
            let
                debug = log "SelectRecipeMouse" id
                newModel =
                    setQuery model id
                        |> resetMenu
            in
                ( newModel, Task.attempt (\_ -> NoOp) (Dom.focus "recipe-day-input") )
        SetQuery newQuery ->
            let
                showMenu =
                    not << List.isEmpty <| searchRecipe newQuery
            in
                {model | query = newQuery, showMenu = showMenu, selectedRecipe = Nothing } ! []
        SetAutoState autoMsg ->
            let
                
                (newState, maybeMsg ) =
                    Autocomplete.update updateConfig autoMsg model.showHowMany model.autoState (searchRecipe model.query)
                newModel =
                    {model | autoState = newState}
            in
              case maybeMsg of
                Nothing ->
                    newModel ! []
                Just updateMsg ->
                    update updateMsg newModel
        PreviewRecipe id ->
            {model | selectedRecipe = Just <| (getRecipe id)} ! []

        Wrap toTop ->
            case model.selectedRecipe of
                Just recipe ->
                    update Reset model
                Nothing ->
                    if toTop then
                        { model
                            | autoState = Autocomplete.resetToLastItem updateConfig (searchRecipe model.query) model.showHowMany model.autoState
                            , selectedRecipe = List.head <| List.reverse <| List.take model.showHowMany <| (searchRecipe model.query )
                        }
                            ! []
                    else
                        { model
                            | autoState = Autocomplete.resetToFirstItem updateConfig (searchRecipe model.query) model.showHowMany model.autoState
                            , selectedRecipe = List.head <| List.take model.showHowMany <| (searchRecipe model.query)
                        }
                            ! []
        Reset ->
            { model | autoState = Autocomplete.reset updateConfig model.autoState, selectedRecipe = Nothing } ! []
        HandleEscape ->
            let
                validOptions =
                    not <| List.isEmpty (searchRecipe model.query)

                handleEscape =
                    if validOptions then
                        model
                            |> removeSelection
                            |> resetMenu
                    else
                        { model | query = "" }
                            |> removeSelection
                            |> resetMenu

                escapedModel =
                    case model.selectedRecipe of
                        Just person ->
                            if model.query == person.name then
                                model
                                    |> resetInput
                            else
                                handleEscape

                        Nothing ->
                            handleEscape
            in
                escapedModel ! [] 
        OnFocus ->
            model ! []
              
resetInput : Model -> Model
resetInput model =
    { model | query = "" }
        |> removeSelection
        |> resetMenu

removeSelection : Model -> Model
removeSelection model =
    { model | selectedRecipe = Nothing }

resetMenu : Model -> Model
resetMenu model =
        { model
            | autoState = Autocomplete.empty
            , showMenu = False
        }

updateConfig : Autocomplete.UpdateConfig Msg Recipe
updateConfig =
    let
      test = ""
    in
        Autocomplete.updateConfig
            { toId = .name
            , onKeyDown =
                \code maybeId ->
                    if code == 38 || code == 40 then
                        Maybe.map PreviewRecipe maybeId
                    else if code == 13 then
                        Maybe.map SelectRecipeKeyboard maybeId
                    else
                        Just <| Reset
            , onTooLow = Just <| Wrap False
            , onTooHigh = Just <| Wrap True
            , onMouseEnter = \id -> Just <| PreviewRecipe id
            , onMouseLeave = \_ -> Nothing
            , onMouseClick = \id -> Just <| SelectRecipeMouse id
            , separateSelections = False
            }


setQuery : Model -> String -> Model
setQuery model id =
    { model 
        | query = .name <| getRecipe id
        , selectedRecipe = Just <| getRecipe id
    }

editDayRecipeInWeek : Week -> Int -> Bool -> Week
editDayRecipeInWeek week dayId isEditing =
    let
        updateDay day =
        if (day.id == dayId) then
            { day | editing = isEditing }
        else
            day
         --   { day | editing = False }
        updateWeek week =
        {week | days = List.map updateDay week.days}
    in
        updateWeek week
        

updateDayRecipeInWeek : Week -> Int -> Recipe -> Week
updateDayRecipeInWeek week dayId recipe =
    let
        newWeek = {week | days = List.map updateDay week.days}
        oldDay = Maybe.withDefault (initDay dayId) (find (\n -> n.id == dayId) model.week.days)
        updateDay d = 
            if (d.id == dayId) then 
                {d | recipe = recipe}
            else
                d
    in
        newWeek

recipes : List Recipe
recipes =
    [ { name = "Pasta", editing = False, selected = False }
    , { name = "Taco", editing = False, selected = False }
    , { name = "Lasagne", editing = False, selected = False }
    , { name = "Tandoori", editing = False, selected = False }
    , { name = "Pizza White", editing = False, selected = False }
    , { name = "Pizza Red", editing = False, selected = False }
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

onTab : Msg -> Attribute Msg
onTab msg =
    let 
        isTab code =
            if code == 9 then
                Json.succeed msg
            else
                Json.fail "not tab key"
    in
        on "keydown" (Json.andThen isTab keyCode)


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

getRecipe : String -> Recipe
getRecipe name =
    let
      recipe = find (\n -> n.name == name) recipes
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
        div [ class "main", style [ ( "margin-top", "30px" ), ( "text-align", "center" ) ] ]
            [
              div []
                [ div [] [ calender model ] 
                ]
            ]

viewRecipeDayInput : Model -> Day -> List (Html Msg)
viewRecipeDayInput model day =
    let
        options =
            { preventDefault = True, stopPropagation = False }

        dec =
            (Json.map
                (\code ->
                    if code == 38 || code == 40 then
                        Ok NoOp
                    else if code == 27 then
                        Ok HandleEscape
                    else
                        Err "not handling that key"
                )
                keyCode
            )
                |> Json.andThen
                    fromResult
        fromResult : Result String a -> Json.Decoder a
        fromResult result =
            case result of
                Ok val ->
                    Json.succeed val
                Err reason ->
                    Json.fail reason
        menu =
            if model.showMenu then
                [ viewMenu model ]
            else
                []
        query =
            case model.selectedRecipe of
                Just recipe ->
                    recipe.name

                Nothing ->
                    model.query
        activeDescendant attributes =
            case model.selectedRecipe of
                Just recipe ->
                    (attribute "aria-activedescendant"
                        recipe.name
                    )
                        :: attributes

                Nothing ->
                    attributes
    in
        List.append
            [ input 
                (activeDescendant
                    [ placeholder "recipe name"
                    , autofocus True
                    , onInput SetQuery
                    , onFocus OnFocus
                    , onWithOptions "keydown" options dec
                    , onBlur (EditDayRecipe day.id False)
                    , onEnter (EditDayRecipe day.id False)
                    , onTab (EditNextDayRecipe day.id (day.id + 1))
                    , value query
                    , id ("recipe-day-input-" ++ toString day.id)
                    , class "autocomplete-input"
                    , autocomplete False
                    , attribute "aria-owns" "list-of-recipes"
                    , attribute "aria-expanded" <| String.toLower <| toString model.showMenu
                    , attribute "aria-haspopup" <| String.toLower <| toString model.showMenu
                    , attribute "role" "combobox"
                    , attribute "aria-autocomplete" "list"
                    ]
                )
                []
            ]
            menu

viewMenu : Model -> Html Msg
viewMenu model =
    div [ class "autocomplete-menu" ]
        [ Html.map SetAutoState (Autocomplete.view viewConfig model.showHowMany model.autoState (searchRecipe model.query)) ]

viewConfig : Autocomplete.ViewConfig Recipe
viewConfig =
    let
        customizedLi keySelected mouseSelected recipe =
            { attributes =
                [ classList [ ( "autocomplete-item", True ), ( "key-selected", keySelected || mouseSelected ) ]
                , id recipe.name
                ]
            , children = [ Html.text recipe.name ]
            }
    in
        Autocomplete.viewConfig
            { toId = .name
            , ul = [ class "autocomplete-list" ]
            , li = customizedLi
            }

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
    option [ Html.Attributes.value (toString recipe.name) ] [ text (recipe.name) ]

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
            [ class "recipe edit"]
            (viewRecipeDayInput model day)
        ]
