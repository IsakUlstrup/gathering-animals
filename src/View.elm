module View exposing (viewAnimal, viewInventory, viewLocation, viewLoot, viewResource)

import Engine.Animal as Animal exposing (Animal)
import Engine.Inventory exposing (Inventory, ItemStack)
import Engine.Item as Item exposing (Item)
import Engine.Resource as Resource exposing (Resource)
import Html exposing (Attribute, Html, button, div, h3, p, text)
import Html.Attributes exposing (class, classList)
import Html.Events exposing (onClick)


viewAnimal : Animal -> Html msg
viewAnimal animal =
    div
        [ class "animal" ]
        [ p [ classList [ ( "action", Animal.isInteracting animal ) ] ] [ text "🐮" ] ]


viewLoot : (Int -> msg) -> List Item -> Html msg
viewLoot lootEvent items =
    let
        lootBtn : Int -> Item -> Html msg
        lootBtn index i =
            button [ class "loot-button", class "emoji", onClick (lootEvent index) ] [ text <| Item.iconString i ]
    in
    div [ class "loot-list" ] (List.indexedMap lootBtn items)


viewResource : Resource -> Html msg
viewResource resource =
    div
        [ class "resource"
        , classList
            [ ( "evade", Resource.isEvade resource )
            , ( "hit", Resource.isHit resource )
            , ( "exhausted", Resource.isExhausted resource )
            ]
        ]
        [ p [ class "resource-icon", class "emoji" ] [ text "🌲" ] ]



-- viewResource : (Int -> msg) -> msg -> Resource -> Html msg
-- viewResource lootEvent resetEvent resource =
--     div [ class "resource-container" ]
--         [ case Resource.getLoot resource of
--             Just loot ->
--                 div [ class "loot" ]
--                     [ div [ class "loot-list" ] (List.indexedMap (viewLoot lootEvent) loot)
--                     , button [ class "regrow-button", onClick resetEvent ] [ text "Done" ]
--                     ]
--             Nothing ->
--                 div [ class "loot", class "no-loot" ] []
--         , div
--             [ class "resource"
--             , classList
--                 [ ( "evade", Resource.isEvade resource )
--                 , ( "hit", Resource.isHit resource )
--                 , ( "exhausted", Resource.isExhausted resource )
--                 ]
--             ]
--             [ p [ class "resource-icon", class "emoji" ] [ text "🌲" ] ]
--         ]


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
