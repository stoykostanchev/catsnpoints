module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import String exposing (fromFloat, fromInt)


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }


type alias Category =
    { name : String
    }


dummyCategory : Category
dummyCategory =
    { name = "Work"
    }


type alias EntryType =
    { name : String
    , min : Float
    , max : Float
    }


dummyEntryType : EntryType
dummyEntryType =
    { name = "WorkEntryType"
    , min = 0
    , max = 10
    }


type alias Entry =
    { kind : EntryType
    , value : Float
    , category : Category
    }


dummyEntry : Entry
dummyEntry =
    Entry dummyEntryType 3 dummyCategory


type alias Model =
    { categories : List Category
    , newCategory : Maybe Category
    , activeCategory : Maybe Category
    , entries : List Entry
    , newEntry : Entry -- Perhaps conceptually it is fine for us to ALWAYS have a blank entry?
    }


type Msg
    = AddCategory
    | CategoryInput String
    | RemoveCategory Category
    | AddEntry Entry
    | RemoveEntry Entry
    | EntryKindInput EntryType
    | EntryValueInput Float
    | EntryCategoryInput Category



-- On updating nested modeles: https://medium.com/elm-shorts/updating-nested-records-in-elm-15d162e80480


update : Msg -> Model -> Model
update msg model =
    let
        oldEntry =
            model.newEntry
    in
        case msg of
            AddEntry entry ->
                ( model )

            EntryKindInput kind ->
                let
                    newEntry =
                        { oldEntry | kind = kind }
                in
                    ( model
                      --( { model | newEntry = Just newEntry}
                    )

            EntryValueInput value ->
                let
                    newEntry =
                        { oldEntry | value = value }
                in
                    -- ( { model | newEntry = Just newEntry}
                    ( model
                    )

            EntryCategoryInput category ->
                let
                    newEntry =
                        { oldEntry | category = category }
                in
                    ( model
                      -- ( { model | newEntry = newEntry}
                    )

            CategoryInput input ->
                ( { model | newCategory = Just (Category input) }
                )

            AddCategory ->
                case model.newCategory of
                    Just category ->
                        ( { model | categories = category :: model.categories, newCategory = Nothing }
                        )

                    Nothing ->
                        ( model )

            RemoveCategory category ->
                ( { model | categories = List.filter (\c -> c /= category) model.categories }
                )

            RemoveEntry entry ->
                ( { model | entries = List.filter (\e -> e /= entry) model.entries }
                )


categoryView : Category -> Html Msg
categoryView category =
    div []
        [ text category.name
        , button [ onClick (RemoveCategory category) ] [ text "Remove" ]
        ]


view : Model -> Html Msg
view model =
    div []
        [ categoriesView model
        , entriesView model
        ]


entryView : Entry -> Html Msg
entryView entry =
    div []
        [ span [] [ text ("Value: " ++ (fromFloat entry.value) ++ " ") ]
        , span [] [ text ("Type (name): " ++ entry.kind.name) ]
        , span [] [ text ("Category (name): " ++ entry.category.name) ]
        , span []
            [ input [ type_ "button", onClick (RemoveEntry entry), value "Remove" ] []
            ]
        ]


entriesView : Model -> Html Msg
entriesView model =
    div []
        [ h1 [] [ text "Entries" ]
        , div [] (List.map entryView model.entries)
        , div [] [ (addEntryView model.newEntry) ]
        ]


addEntryView : Entry -> Html Msg
addEntryView entry =
    div
        [ ]
        [ input [ type_ "number", placeholder "- Entry value -", value (fromFloat entry.value) ] []
        ]


categoriesView : Model -> Html Msg
categoriesView model =
    let
        newCatInputVal =
            case model.newCategory of
                Just category ->
                    category.name

                Nothing ->
                    ""
    in
        div []
            [ h1 [] [ text "Categories:" ]
            , div [] (List.map categoryView model.categories)
            , input [ type_ "text", placeholder "- Category name -", value newCatInputVal, onInput CategoryInput ] []
            , button [ onClick AddCategory ] [ text "Add" ]
            ]



init : Model
init = { categories = [ dummyCategory ]
       , activeCategory = Just dummyCategory
       , entries = []
       , newCategory = Nothing
       , newEntry = dummyEntry
       }