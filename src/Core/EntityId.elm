module Core.EntityId exposing (..)


type EntityId
    = EntityId Int


fromInt : Int -> EntityId
fromInt =
    EntityId


toString : EntityId -> String
toString (EntityId id) =
    String.fromInt id
