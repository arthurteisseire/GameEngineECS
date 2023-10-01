module Core.Table exposing (..)

import Core.EntityId as EntityId exposing (EntityId(..))
import Dict exposing (Dict)
import Dict.OneToOne


type Table a
    = Table (Dict Int a)



-- Helpers


fromList : List ( Int, a ) -> Table a
fromList list =
    list
        |> Dict.fromList
        |> fromDict


toList : Table a -> List ( EntityId, a )
toList table =
    foldl (\key value list -> ( key, value ) :: list) [] table


fromDict : Dict Int a -> Table a
fromDict dict =
    Dict.foldl
        (\id a table -> insert (EntityId.fromInt id) a table)
        empty
        dict


hasValue : a -> Table a -> Bool
hasValue =
    hasValueIf (==)


hasValueIf : (a -> b -> Bool) -> a -> Table b -> Bool
hasValueIf predicate a table =
    filter
        (\_ value -> predicate a value)
        table
        /= empty


mapRow : EntityId -> (a -> result) -> Table a -> Maybe result
mapRow entityId func table =
    Maybe.map func (get entityId table)



-- Dict.OneToOne Bindings


select : a -> a
select =
    Dict.OneToOne.select


from : Table a -> (a -> result) -> Table result
from (Table dict) func =
    Dict.OneToOne.from dict func
        |> Table


innerJoin : Table a -> Table (a -> result) -> Table result
innerJoin (Table dict) (Table nextDict) =
    Dict.OneToOne.innerJoin dict nextDict
        |> Table


outerLeftJoin : Table a -> Table (Maybe a -> result) -> Table result
outerLeftJoin (Table dict) (Table nextDict) =
    Dict.OneToOne.leftOuterJoin dict nextDict
        |> Table



-- Dict Bindings


empty : Table a
empty =
    Table Dict.empty


get : EntityId -> Table a -> Maybe a
get (EntityId id) (Table dict) =
    Dict.get id dict


filter : (EntityId -> a -> Bool) -> Table a -> Table a
filter isGood (Table dict) =
    Table (Dict.filter (\id a -> isGood (EntityId.fromInt id) a) dict)


insert : EntityId -> a -> Table a -> Table a
insert (EntityId id) a (Table dict) =
    Table (Dict.insert id a dict)


remove : EntityId -> Table a -> Table a
remove (EntityId id) (Table dict) =
    Table (Dict.remove id dict)


foldl : (EntityId -> a -> b -> b) -> b -> Table a -> b
foldl func acc (Table dict) =
    Dict.foldl (\id a b -> func (EntityId.fromInt id) a b) acc dict


keys : Table a -> List EntityId
keys (Table dict) =
    List.map EntityId.fromInt (Dict.keys dict)


values : Table a -> List a
values (Table dict) =
    Dict.values dict


update : EntityId -> (Maybe v -> Maybe v) -> Table v -> Table v
update (EntityId id) func (Table dict) =
    Dict.update id func dict
        |> Table


map : (EntityId -> a -> b) -> Table a -> Table b
map func (Table dict) =
    Dict.map
        (\id -> func (EntityId.fromInt id))
        dict
        |> Table


union : Table a -> Table a -> Table a
union (Table dictHighPriority) (Table dictLowPriority) =
    Table (Dict.union dictHighPriority dictLowPriority)


merge :
    (EntityId -> a -> result -> result)
    -> (EntityId -> a -> b -> result -> result)
    -> (EntityId -> b -> result -> result)
    -> Table a
    -> Table b
    -> result
    -> result
merge leftStep bothStep rightStep (Table leftDict) (Table rightDict) initialResult =
    Dict.merge
        (\id a t -> leftStep (EntityId.fromInt id) a t)
        (\id a b t -> bothStep (EntityId.fromInt id) a b t)
        (\id b t -> rightStep (EntityId.fromInt id) b t)
        leftDict
        rightDict
        initialResult
