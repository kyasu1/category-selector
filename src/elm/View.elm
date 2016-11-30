module View exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode
import Category exposing (Model, Msg(..), Category, CategoryId, ChildCategory(..))


view : Model -> Html Msg
view model =
    div
        [ class "scroll" ]
        [ ul [ class "flex" ]
            (List.map categoryList model.categoryList |> List.reverse)
        ]


modal : Html Msg -> Html Msg
modal elem =
    div [ class "modal is-active" ]
        [ div [ class "modal-background" ] []
        , div [ class "modal-content" ]
            [ elem
            ]
        , button [ class "modal close" ] []
        ]


categoryList : ( Category, Maybe CategoryId ) -> Html Msg
categoryList ( category, selected ) =
    case ( category.isLeaf, category.childCategory ) of
        ( False, Child list ) ->
            li [ class "control", onChange (Selected category) ]
                [ select [ class "select" ]
                    (List.append
                        [ option [ Html.Attributes.value "-1" ] [ text "---" ] ]
                        (List.map (options <| selected) list)
                    )
                ]

        ( True, _ ) ->
            li [] []


onChange : (String -> msg) -> Html.Attribute msg
onChange tagger =
    Html.Events.on "change" (Json.Decode.map tagger Html.Events.targetValue)


options : Maybe CategoryId -> Category -> Html Msg
options selected category =
    case selected of
        Just selected_ ->
            option
                [ Html.Attributes.selected (selected_ == category.categoryId)
                , Html.Attributes.value <| toString category.categoryId
                ]
                [ optionText category ]

        Nothing ->
            option [ Html.Attributes.value <| toString category.categoryId ] [ optionText category ]


optionText : Category -> Html Msg
optionText category =
    case ( category.isLeaf, category.isLink ) of
        ( _, True ) ->
            span [] [ text <| category.categoryName ++ "@" ]

        ( False, _ ) ->
            span [] [ text <| category.categoryName ++ ">" ]

        ( _, _ ) ->
            span [] [ text category.categoryName ]


categoryRow : Category -> Html Msg
categoryRow category =
    option []
        [ a [ onClick <| LoadCategory category.categoryId ]
            [ text category.categoryName ]
        ]
