module Core.Component exposing (..)

import Core.ComponentTable as ComponentTable exposing (ComponentTable)
import Core.EntityId exposing (EntityId)


select : a -> EntityId -> db -> Maybe a
select a _ _ =
    Just a


join : (db -> ComponentTable a) -> (EntityId -> db -> Maybe (a -> b)) -> EntityId -> db -> Maybe b
join getTable nestedFunc entityId db =
    nestedFunc entityId db
        |> Maybe.andThen
            (\func -> ComponentTable.mapRow entityId func (getTable db))


fullSelect : a -> EntityId -> db -> a
fullSelect a _ _ =
    a


fullJoin : (db -> ComponentTable a) -> (EntityId -> db -> Maybe a -> b) -> EntityId -> db -> b
fullJoin getTable previousFunc entityId db =
    previousFunc entityId db (ComponentTable.get entityId (getTable db))
