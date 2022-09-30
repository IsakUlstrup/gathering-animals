port module Storage exposing (inventoryDecoder, saveInventory)

import Engine.Item exposing (Item)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)


port storeInventory : String -> Cmd msg



-- ENCODE


saveInventory : List Item -> Cmd msg
saveInventory items =
    Encode.list itemEncoder items
        |> Encode.encode 0
        |> storeInventory


itemEncoder : Item -> Value
itemEncoder item =
    Encode.object
        [ ( "icon", Encode.string <| String.fromChar item.icon )
        ]



-- DECODE


itemDecoder : Decoder Item
itemDecoder =
    let
        charHelper : String -> Decoder Char
        charHelper s =
            case String.toList s of
                [ c ] ->
                    Decode.succeed c

                _ ->
                    Decode.fail "Invalid char"
    in
    Decode.map Engine.Item.new
        (Decode.field "icon" Decode.string
            |> Decode.andThen charHelper
        )


inventoryDecoder : Decoder (List Item)
inventoryDecoder =
    Decode.list itemDecoder
