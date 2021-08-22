module Model.Zipped.Fixtures exposing (..)

import Model.EdgeData exposing (EdgeData)
import Model.VertexData exposing (Address, VertexData)
import Model.Zipped exposing (Zipped)


zipped1 : List Zipped
zipped1 =
    [ ( edge1, vendor1 )
    , ( edge2, vendor2 )
    , ( edge3, vendor3 )
    ]


zipped4 : List Zipped
zipped4 =
    [ ( edge7, vendor4 )
    , ( edge8, vendor5 )
    , ( edge9, vendor6 )
    , ( edge10, vendor7 )
    , ( edge11, vendor8 )
    ]


edge1 : EdgeData
edge1 =
    EdgeData
        "1C"
        "1V"
        "10 transactions"
        "$100"
        "$10"
        "$10"
        "$10"


edge2 : EdgeData
edge2 =
    { edge1 | dst_id = "2V" }


edge3 : EdgeData
edge3 =
    { edge1 | dst_id = "3V" }


edge4 : EdgeData
edge4 =
    EdgeData
        "2C"
        "2V"
        "10 transactions"
        "$400"
        "$40"
        "$40"
        "$40"


edge5 : EdgeData
edge5 =
    { edge4 | dst_id = "3V" }


edge6 : EdgeData
edge6 =
    { edge4 | dst_id = "4V" }


edge7 : EdgeData
edge7 =
    EdgeData
        "1C"
        "4V"
        "10 transactions"
        "$700"
        "$70"
        "$70"
        "$70"


edge8 : EdgeData
edge8 =
    { edge7 | dst_id = "5V" }


edge9 : EdgeData
edge9 =
    { edge7 | dst_id = "6V" }


edge10 : EdgeData
edge10 =
    { edge7 | dst_id = "7V" }


edge11 : EdgeData
edge11 =
    { edge7 | dst_id = "8V" }


vendor1 : VertexData
vendor1 =
    { uid = "1V"
    , name = "Vendor1"
    , alternate_names = []
    , is_committee = True
    , address = address1
    , alternate_addresses = []
    }


address1 : Address
address1 =
    { alternate_street = Nothing
    , city = Nothing
    , state = Nothing
    , street = Nothing
    , zip_code = Nothing
    }


vendor2 : VertexData
vendor2 =
    { vendor1 | uid = "2V" }


vendor3 : VertexData
vendor3 =
    { vendor1 | uid = "3V" }


vendor4 : VertexData
vendor4 =
    { vendor1 | uid = "4V" }


vendor5 : VertexData
vendor5 =
    { vendor1 | uid = "5V" }


vendor6 : VertexData
vendor6 =
    { vendor1 | uid = "6V" }


vendor7 : VertexData
vendor7 =
    { vendor1 | uid = "7V" }


vendor8 : VertexData
vendor8 =
    { vendor1 | uid = "8V" }
