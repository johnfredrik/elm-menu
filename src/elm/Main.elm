module Main exposing (..)
import Dom
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json
import Task

-- APP
main =
  Html.program
  { init = init
  , view = view
  , update = update
  , subscriptions = \_ -> Sub.none
  }

init: (Model, Cmd Msg)
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
  , weeks = [(initWeek 0), (initWeek 1), (initWeek 2)]
  , recipes = []
  , recipeName = ""
  }

type alias Recipe = 
  { id : Int
  , name : String
  , editing : Bool
  }

type alias Day = {id: Int, recipe : Recipe, editable: Bool}

type alias Week = 
  { number : Int
  , monday : Day
  , tuesday : Day
  , wednesday: Day
  , thursday : Day
  , friday : Day
  , saturday : Day
  , sunday : Day
  }

initWeek : Int -> Week
initWeek number =
  { number = number
  , monday = {id = 0, recipe = initRecipe, editable = False}
  , tuesday = {id = 1, recipe = initRecipe, editable = False}
  , wednesday = {id = 2, recipe = initRecipe, editable = False}
  , thursday = {id = 3, recipe = initRecipe, editable = False}
  , friday = {id = 4, recipe = initRecipe, editable = False}
  , saturday = {id = 5, recipe = initRecipe, editable = False}
  , sunday = {id = 6, recipe = initRecipe, editable = False}
  }
initRecipe : Recipe
initRecipe =
  { id = -1, name = "", editing = False}

newRecipe : Int -> String -> Recipe
newRecipe id name =
  { id = id, name = name, editing = False}
-- UPDATE
type Msg 
  = NoOp
  | AddRecipe
  | UpdateRecipe Int String
  | EditRecipe Int Bool
  | SetRecipeMonday Week String
  | UpdateRecipeName String

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    NoOp -> model ! []

    UpdateRecipeName name ->
      {model | recipeName = name} ! []

    AddRecipe ->
      { model 
        | recipeId = model.recipeId + 1
        , recipeName = ""
        , recipes =
          if String.isEmpty model.recipeName then 
            model.recipes
          else
            model.recipes ++ [ newRecipe model.recipeId model.recipeName ]
       } ! []
    UpdateRecipe id name ->
      let
        updateRecipe recipe =
          if recipe.id == id then
            { recipe | name = name }
          else 
            recipe
      in
        {model | recipes = List.map updateRecipe model.recipes} ! []
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
        {model | recipes = List.map updateRecipe model.recipes} ! []
    SetRecipeMonday week value ->
      let
        number n =
          case n of 
            Err n ->
              0
            Ok n ->
              n
        mabyRecipe = find (\n -> n.id == (number (String.toInt value))) recipes
        recipe r =
          case r of 
            Nothing ->
              initRecipe
            Just r ->
              r
        newWeek = {week | monday = (recipe mabyRecipe)}
      in
        model ! []

updateWeek : List Week -> Week -> List Week
updateWeek weeks week =
  let
    filteredWeeks = List.filter (\n -> n.number == week.number) weeks
  in
    List.sortBy .number (week::filteredWeeks)

recipes : List Recipe
recipes =
  [ { id = 1, name = "Pasta", editing = False}
  , { id = 2, name = "Taco" , editing = False}
  , { id = 3, name= "Lasagne" , editing = False}
  , { id = 4, name ="Tandoori" , editing = False}
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

    first::rest ->
      if predicate first then 
        Just first
      else
        find predicate rest

-- VIEW
-- Html is defined as: elem [ attribs ][ children ]
-- CSS can be applied via class names or inline style attrib
view : Model -> Html Msg
view model =
  div [ class "container", style [("margin-top", "30px"), ( "text-align", "center" )] ][    -- inline CSS (literal)
    div [ class "row" ]
    [ div [ class "col-md-2" ] 
      [ h5 [] [ text "Recipes:" ]
      , input 
        [ placeholder "Add new recipe"
        , value model.recipeName
        , onInput UpdateRecipeName
        , onEnter AddRecipe
        ] []
     ,  recipeList model.recipes
     ]
    , div [ class "col-md-10" ] [calender model]
    ]
  ]
calenderHeader : Html Msg
calenderHeader =
  div [ class "col-md-12"]
    [ h5 [class "col-md-1"][ text "Week" ]
    , h5 [ class "col-md-1"][ text "Mon"]
    , h5 [ class "col-md-1"][ text "Tue"]
    , h5 [ class "col-md-1"][ text "Wed"]
    , h5 [ class "col-md-1"][ text "Thu"]
    , h5 [ class "col-md-1"][ text "Fri"]
    , h5 [ class "col-md-1"][ text "Sat"]
    , h5 [ class "col-md-1"][ text "Sun"]
    ]

calender : Model -> Html Msg
calender model = 
  div []
  [ calenderHeader
  , viewWeeks model
  ]

recipeOption : Recipe -> Html Msg
recipeOption recipe =
  option [ Html.Attributes.value (toString recipe.id)] [ text (recipe.name)]


recipeSingle : Recipe -> Html Msg
recipeSingle recipe =
  li [ classList [("editing", recipe.editing) ] ] 
    [ div 
      [ class "view"] 
      [ label 
        [ onDoubleClick (EditRecipe recipe.id True) ] 
        [ text recipe.name] ]
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
    div [ class "col-md-12 week"]
    [ div [ class "col-md-1 week-number"] [ text (toString week.number)]
    , viewDay model week.monday
    , viewDay model week.tuesday
    , viewDay model week.wednesday
    , viewDay model week.thursday
    , viewDay model week.friday
    , viewDay model week.saturday
    , viewDay model week.sunday
    ]

viewDay : Model -> Day -> Html Msg
viewDay model day =
    div 
      [ classList [ ("col-md-1", True), ("recipe-box", True), ("editable", day.editable)]] 
      [ div 
        [ class "view"] 
        [ label 
          [ ] 
          [ text day.recipe.name] ]
      , input 
        [ class "edit"
        , value day.recipe.name
        ]
        []
      ]