module Cart where

data Article 
    = Lifestyle
    | Furniture

data ShippingAddress
    = HomeAddress
    | PackStation

data Tentative reason a
    = Valid a
    | Invalid a [reason]

instance Functor (Tentative reason) where
    fmap f (Valid a) = Valid (f a)
    fmap f (Invalid a reason) = Invalid (f a) reason

instance Applicative (Tentative reason) where
    pure = Valid
    (Valid f) <*> (Valid a) = Valid (f a)
    (Invalid f reason1) <*> (Invalid a reason2) =
        Invalid (f a) (reason1 ++ reason2)

type TentativeShippingAddress =
    Tentative ReasonForInvalidShippingAddress ShippingAddress

data ReasonForInvalidShippingAddress =
    NoFurnitureToPackStation

checkShippingAddress :: ShippingAddress -> Article -> TentativeShippingAddress
checkShippingAddress PackStation Furniture =
    Invalid PackStation NoFurnitureToPackStation
checkShippingAddress address _ =
    Valid address
