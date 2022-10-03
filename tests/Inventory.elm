module Inventory exposing (add)

import Content.Items as Items
import Engine.Inventory as Inventory
import Expect
import Test exposing (Test, describe, test)


add : Test
add =
    describe "Add items"
        [ test "Add item to empty inventory" <|
            \_ ->
                Inventory.empty
                    |> Inventory.add Items.coconut
                    |> Expect.equal
                        [ { item = Items.coconut, amount = 1 } ]
        , test "Add two of the same item to empty inventory" <|
            \_ ->
                Inventory.empty
                    |> Inventory.add Items.coconut
                    |> Inventory.add Items.coconut
                    |> Expect.equal
                        [ { item = Items.coconut, amount = 2 } ]
        , test "Add two different items to empty inventory, last item added should be first" <|
            \_ ->
                Inventory.empty
                    |> Inventory.add Items.coconut
                    |> Inventory.add Items.mango
                    |> Expect.equal
                        [ { item = Items.mango, amount = 1 }, { item = Items.coconut, amount = 1 } ]
        ]
