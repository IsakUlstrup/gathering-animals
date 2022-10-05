module View.MainView exposing
    ( flexCenter
    , locationStyle
    , viewAnimal
    , viewInventory
    , viewResource
    )

import Css exposing (Style, px, rgb)
import Css.Animations
import Css.Transitions
import Engine.Animal as Animal exposing (Animal)
import Engine.Inventory exposing (Inventory, ItemStack)
import Engine.Item exposing (Item)
import Engine.Resource as Resource exposing (Resource)
import Html.Styled exposing (Html, button, div, h3, p, text)
import Html.Styled.Attributes exposing (class, css)
import Html.Styled.Events
import View.CssExtra
import View.Paper


viewInventory : Inventory -> Html msg
viewInventory inventory =
    let
        viewItemStack : ItemStack -> Html msg
        viewItemStack stack =
            div [ class "item" ] [ text <| Engine.Item.iconString stack.item ++ "x" ++ String.fromInt stack.amount ]
    in
    div
        [ css
            [ Css.padding <| Css.rem 1
            , Css.displayFlex
            , Css.flexDirection Css.column
            , View.CssExtra.gap 0.5
            ]
        ]
        [ h3 [] [ text "Inventory" ]
        , div
            [ class "items"
            , css
                [ Css.displayFlex
                , Css.flexWrap Css.wrap
                , View.CssExtra.gap 1
                ]
            ]
            (List.map viewItemStack inventory)
        ]


viewAnimal : Animal -> Html msg
viewAnimal animal =
    let
        state : List Css.Style
        state =
            if Animal.isInteracting animal then
                [ Css.transform <| Css.translate2 (px 100) (px 0) ]

            else
                []

        enterAnimation : Css.Animations.Keyframes {}
        enterAnimation =
            Css.Animations.keyframes
                [ ( 0, [ View.CssExtra.maxWidthAnim 0 ] )
                , ( 100, [ View.CssExtra.maxWidthAnim 200 ] )
                ]
    in
    div
        [ css
            [ Css.flex (Css.int 1)
            , Css.Transitions.transition [ Css.Transitions.transform 1000 ]
            , View.Paper.paperSolid <| rgb 255 255 200
            , flexCenter
            , Css.animationName enterAnimation
            , Css.animationDuration <| Css.ms 1500
            ]
        ]
        [ h3 [ css (Css.Transitions.transition [ Css.Transitions.transform 1000 ] :: state) ] [ text "Animal" ] ]


viewLoot : (Int -> msg) -> Int -> Item -> Html msg
viewLoot lootEvent index item =
    button
        [ Html.Styled.Events.onClick <| lootEvent index
        , css
            [ Css.padding <| Css.rem 1
            , Css.fontSize <| Css.rem 1
            , Css.backgroundImage Css.none
            , Css.backgroundColor Css.transparent
            , Css.flexWrap Css.wrap

            -- , Css.display Css.block
            , Css.border <| Css.rem 0

            -- , Css.borderRadius <| Css.rem 9999
            -- , Css.lineHeight <| Css.rem 1
            ]
        ]
        [ text <| Engine.Item.iconString item ]


viewResource : (Int -> msg) -> msg -> Resource -> Html msg
viewResource lootEvent resetEvent resource =
    div
        [ css
            [ Css.flex <| Css.int 1
            , flexCenter
            ]
        ]
        [ div []
            [ h3 [] [ text "Resource" ]
            , p []
                (if Resource.isAlive resource then
                    [ p [] [ text "done" ]
                    ]

                 else if Resource.isRegrowing resource then
                    [ text "Regrowing "
                    ]

                 else if Resource.isHit resource then
                    [ p [ class "hit" ] [ text "hit" ]
                    ]

                 else if Resource.isEvade resource then
                    [ p [ class "evade" ] [ text "evade" ]
                    ]

                 else
                    case Resource.getLoot resource of
                        Just items ->
                            [ text "exhausted"
                            , div [ css [ Css.displayFlex, View.CssExtra.gap 0.5 ] ] (List.indexedMap (viewLoot lootEvent) items)
                            , button [ Html.Styled.Events.onClick resetEvent, css [ Css.fontSize (Css.rem 1) ] ] [ text "Done" ]
                            ]

                        Nothing ->
                            [ text "exhausted, no loot"
                            , button [ Html.Styled.Events.onClick resetEvent ] [ text "Done" ]
                            ]
                )
            ]
        ]


locationStyle : Style
locationStyle =
    Css.batch
        [ Css.displayFlex
        , Css.flex3 (Css.int 1) (Css.int 1) (Css.int 0)
        , Css.minHeight (Css.rem 10)
        , Css.justifyContent Css.center
        , Css.padding2 (Css.rem 1) (Css.rem 0)
        ]


flexCenter : Style
flexCenter =
    Css.batch
        [ Css.displayFlex
        , Css.justifyContent Css.center
        , Css.alignItems Css.center
        ]
