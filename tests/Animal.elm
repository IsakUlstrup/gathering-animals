module Animal exposing (..)

import Engine.Animal as Animal
import Expect
import Test exposing (Test, describe, test)


constructor : Test
constructor =
    describe "Animal Constructor tests"
        [ test "Create new animal" <|
            \_ ->
                Animal.new
                    |> .state
                    |> Expect.equal Animal.cooldownState
        ]
