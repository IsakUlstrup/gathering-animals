module Content.Resources exposing (evergreen)

import Content.Items
import Engine.Resource as Resource exposing (Resource)


evergreen : Resource
evergreen =
    Resource.new '🌲'
        '🌱'
        [ ( 1, Content.Items.mango )
        , ( 9, Content.Items.coconut )
        , ( 90, Content.Items.strawberry )
        ]
