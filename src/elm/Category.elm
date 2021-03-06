module Category exposing (..)

import Http
import Task
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (decode, required, optional)


token =
    "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJleHAiOjE0ODA1MjA5MTQsImlkIjoia3lhc3V5YWtvQGdtYWlsLmNvbSIsIm9yaWdfaWF0IjoxNDgwNTE3MzE0fQ.Ip8gXHzZvpD7AVeHhvFM7sDQunVLlIdOVWvux6qL_o8"


host =
    "http://pro-3.local:4000"


type Msg
    = UpdateCategory (Result Http.Error Category)
    | UpdateCategories (Result Http.Error (List Category))
    | LoadCategory CategoryId
    | Selected Category String



-- https://github.com/elm-lang/elm-compiler/blob/0.18.0/hints/recursive-alias.md


type ChildCategory
    = Child (List Category)


type alias CategoryId =
    Int


type alias Category =
    { categoryId : CategoryId
    , categoryName : String
    , categoryPath : String
    , categoryIdPath : String
    , isLeaf : Bool
    , depth : Int
    , order : Int
    , isLink : Bool
    , isLeafToLink : Bool
    , childCategoryNum : Int
    , childCategory : ChildCategory
    }


type alias Model =
    { categoryId : CategoryId
    , categoryList : List ( Category, Maybe CategoryId )
    }


ajax : CategoryId -> Http.Request Category
ajax id =
    Http.request
        { method = "GET"
        , headers =
            [ Http.header "authorization" ("Bearer " ++ token)
            ]
        , url = host ++ "/api/categoryTree/" ++ toString id
        , body = Http.emptyBody
        , expect = Http.expectJson decodeJson
        , timeout = Nothing
        , withCredentials = False
        }


initialModel : ( Model, Cmd Msg )
initialModel =
    ( { categoryId = 0
      , categoryList = []
      }
      --    , getCategory 0
    , linkTo 0
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case Debug.log "msg" msg of
        --    case msg of
        UpdateCategory (Ok category) ->
            let
                list =
                    List.filter (\item -> (.depth (Tuple.first item)) < category.depth) model.categoryList
            in
                ( { model | categoryList = ( category, Nothing ) :: list }
                , Cmd.none
                )

        UpdateCategory (Err error) ->
            model ! []

        UpdateCategories (Ok categories) ->
            ( { model | categoryList = setSelected (List.reverse categories) Nothing }, Cmd.none )

        UpdateCategories (Err error) ->
            model ! []

        LoadCategory id ->
            ( model, getCategory id )

        Selected category id ->
            let
                id_ =
                    String.toInt id |> Result.toMaybe |> Maybe.withDefault 0

                selected =
                    case category.childCategory of
                        Child categoryList ->
                            List.filter (\e -> e.categoryId == id_) categoryList |> List.head
            in
                case Debug.log "Selected" selected of
                    Just category ->
                        if category.isLink == True then
                            ( model, linkTo id_ )
                        else
                            { model
                                | categoryList = List.map (setCategoryId (category.depth - 1) id_) model.categoryList
                            }
                                ! [ getCategory id_ ]

                    Nothing ->
                        { model | categoryList = List.drop (List.length model.categoryList - category.depth - 1) model.categoryList } ! []


setSelected : List Category -> Maybe CategoryId -> List ( Category, Maybe CategoryId )
setSelected categories categoryId =
    case categories of
        [] ->
            []

        head :: tail ->
            ( head, categoryId ) :: (setSelected tail <| Just head.categoryId)


setCategoryId : Int -> CategoryId -> ( Category, Maybe CategoryId ) -> ( Category, Maybe CategoryId )
setCategoryId depth id ( category, categoryId ) =
    if depth == category.depth then
        ( category, Just id )
    else
        ( category, categoryId )


linkTo : CategoryId -> Cmd Msg
linkTo id =
    Http.toTask (ajax id)
        |> Task.andThen
            (\category ->
                category.categoryIdPath
                    |> String.split ","
                    |> List.map (\id -> String.toInt id |> Result.toMaybe |> Maybe.withDefault 0)
                    |> List.map (\id -> Http.toTask <| ajax id)
                    |> Task.sequence
            )
        |> Task.attempt UpdateCategories


getCategory : CategoryId -> Cmd Msg
getCategory id =
    Http.send UpdateCategory (ajax id)


decodeJson : Decode.Decoder Category
decodeJson =
    Decode.field "Result" decodeCategory


decodeCategory : Decode.Decoder Category
decodeCategory =
    decode Category
        |> required "CategoryId" Decode.int
        |> required "CategoryName" Decode.string
        |> required "CategoryPath" Decode.string
        |> required "CategoryIdPath" Decode.string
        |> required "IsLeaf" Decode.bool
        |> required "Depth" Decode.int
        |> required "Order" Decode.int
        |> required "IsLink" Decode.bool
        |> required "IsLeafToLink" Decode.bool
        |> optional "ChildCategoryNum" Decode.int 0
        |> optional "ChildCategory" decodeChildCategory (Child [])



-- https://github.com/elm-lang/elm-compiler/blob/0.18.0/hints/bad-recursion.md


decodeChildCategory : Decode.Decoder ChildCategory
decodeChildCategory =
    Decode.map Child (Decode.list (Decode.lazy (\_ -> decodeCategory)))
