port module Storage exposing (decodeStoredInventory, saveInventory)

import Engine.Item exposing (Item)
import Json.Decode as Decode exposing (Decoder, string)
import Json.Encode as Encode exposing (Value)


port storeInventory : String -> Cmd msg


itemEncoder : Item -> Value
itemEncoder item =
    Encode.object
        [ ( "icon", Encode.string <| String.fromChar item.icon )
        ]



-- <| Engine.Item.toString item
-- itemHelp : String -> Decoder Item
-- itemHelp itemString =
--     case Engine.Item.fromString itemString of
--         Just item ->
--             Decode.succeed item
--         Nothing ->
--             Decode.fail <|
--                 "Invalid item data"


itemHelp2 : String -> Decoder Item
itemHelp2 itemString =
    case String.toList itemString of
        [ i ] ->
            Decode.succeed <| Engine.Item.new i

        _ ->
            Decode.fail <|
                "Invalid Char"


itemDecoder : Decoder Item
itemDecoder =
    Decode.field "icon" string
        |> Decode.andThen itemHelp2


inventoryDecoder : Decoder (List Item)
inventoryDecoder =
    Decode.list itemDecoder


saveInventory : List Item -> Cmd msg
saveInventory items =
    Encode.list itemEncoder items
        |> Encode.encode 0
        |> storeInventory


decodeStoredInventory : String -> Result String (List Item)
decodeStoredInventory inventoryJson =
    Decode.decodeString inventoryDecoder inventoryJson
        |> Result.mapError (always "Error decoding items")
