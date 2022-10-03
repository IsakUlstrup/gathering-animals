module Engine.Inventory exposing (Inventory, ItemStack, add, empty)

import Engine.Item exposing (Item)


type alias ItemStack =
    { item : Item
    , amount : Int
    }


type alias Inventory =
    List ItemStack


{-| Create a new empty inventory
-}
empty : Inventory
empty =
    []


{-| Check if a given items exists in inventory
-}
itemExists : Item -> Inventory -> Bool
itemExists item inventory =
    List.member item (List.map .item inventory)


{-| Increment item stack by 1 if item exists
-}
incAmount : Item -> Inventory -> Inventory
incAmount item inventory =
    let
        helper : ItemStack -> ItemStack
        helper stack =
            if stack.item == item then
                { stack | amount = stack.amount + 1 }

            else
                stack
    in
    List.map helper inventory


{-| Add item to inventory
-}
addItem : Item -> Inventory -> Inventory
addItem item inventory =
    ItemStack item 1 :: inventory


{-| Add item to inventory if one does not already exists. Otherwise increment amount by 1
-}
add : Item -> Inventory -> Inventory
add item inventory =
    if itemExists item inventory then
        incAmount item inventory

    else
        addItem item inventory
