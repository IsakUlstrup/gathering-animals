module View exposing (viewAnimal, viewInventory, viewLocation, viewLoot, viewResource, viewSocial)

import Engine.Animal as Animal exposing (Animal)
import Engine.Inventory exposing (Inventory, ItemStack)
import Engine.Item as Item exposing (Item)
import Engine.Resource as Resource exposing (Resource)
import Html exposing (Attribute, Html, a, button, div, h3, p, text)
import Html.Attributes exposing (class, classList, href, style)
import Html.Events exposing (onClick)


viewAnimal : Animal -> Html msg
viewAnimal animal =
    div
        [ class "animal"
        , classList
            [ ( "action", Animal.isInteracting animal )
            , ( "idle", Animal.isIdle animal )
            ]
        ]
        [ p [ class "emoji", Html.Attributes.title "🐮" ] [ text "🐮" ] ]


viewLoot : (Int -> msg) -> List Item -> Html msg
viewLoot lootEvent items =
    let
        lootBtn : Int -> Item -> Html msg
        lootBtn index i =
            button [ class "loot-button", class "emoji", onClick (lootEvent index) ] [ text <| Item.iconString i ]
    in
    div [ class "loot-list" ] (List.indexedMap lootBtn items)


viewResource : msg -> Resource -> Html msg
viewResource interactMsg resource =
    div
        [ class "resource"
        , classList
            [ ( "alive", Resource.isAlive resource )
            , ( "evade", Resource.isEvade resource )
            , ( "hit", Resource.isHit resource )
            , ( "exhausted", Resource.isExhausted resource )
            , ( "growing", Resource.isRegrowing resource )
            , ( "regrown", Resource.isRegrown resource )
            ]
        ]
        [ p [ onClick interactMsg, class "resource-icon", class "emoji", Html.Attributes.title (Resource.icon resource |> String.fromChar) ] [ text (Resource.icon resource |> String.fromChar) ] ]


viewItemStack : ItemStack -> Html msg
viewItemStack stack =
    div [ class "item-stack" ]
        [ p [ class "item-icon", class "emoji" ] [ text <| String.fromChar stack.item.icon ]
        , p [ class "item-amount" ] [ text <| String.fromInt stack.amount ]
        ]


viewInventory : Inventory -> Html msg
viewInventory inventory =
    div [ class "inventory" ]
        [ h3 [] [ text "Inventory" ]
        , div [ class "item-stacks" ] (List.map viewItemStack inventory)
        ]


viewLocation : List (Attribute msg) -> List (Html msg) -> Html msg
viewLocation attrs content =
    div (class "location" :: attrs) content


viewSocial : Html msg
viewSocial =
    div [ style "text-align" "center", style "display" "flex", style "align-items" "center", style "gap" "2rem", style "flex-direction" "column" ]
        [ p [] [ text "Github: ", a [ href "https://github.com/IsakUlstrup/gathering-animals" ] [ text "gathering-animals" ] ]
        , p [] [ text "This page: ", div [ class "qr-code" ] [] ]
        ]
