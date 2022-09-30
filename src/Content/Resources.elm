module Content.Resources exposing (..)

import Content.Items
import Engine.Resource as Resource exposing (Resource)


test : Resource
test =
    Resource.new
        [ ( 1, Content.Items.mango )
        , ( 9, Content.Items.coconut )
        , ( 90, Content.Items.strawberry )
        ]
