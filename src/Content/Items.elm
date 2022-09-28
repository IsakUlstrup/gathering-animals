module Content.Items exposing (coconut, mango, strawberry)

import Engine.Item as Item exposing (Item)


coconut : Item
coconut =
    Item.new '🥥'


strawberry : Item
strawberry =
    Item.new '🍓'


mango : Item
mango =
    Item.new '🥭'
