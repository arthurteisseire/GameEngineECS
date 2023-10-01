module Core.ComponentTable exposing (..)

import Core.EntityId as EntityId exposing (EntityId)
import Core.EntitySet exposing (EntitySet(..))
import Core.Modifier exposing (Modifier)
import Core.Table as Table exposing (Table)


type ComponentTable a
    = ComponentTable (Table a) (ComponentOps a)


type alias ComponentOps a =
    { toString : a -> String
    }


type alias Ops db =
    { remove : EntityId -> db -> db
    , toString : db -> EntityId -> List String -> List String
    }


toOps : Modifier (ComponentTable a) db -> Ops db
toOps modifier =
    { remove =
        \id currentWorld ->
            modifier.map (remove id) currentWorld
    , toString =
        \world entityId strings ->
            strings
                ++ Maybe.withDefault
                    []
                    (Maybe.map
                        (\comp -> [ (getComponentOps (modifier.get world)).toString comp ])
                        (get entityId (modifier.get world))
                    )
    }


select : a -> a
select =
    Table.select


from : ComponentTable a -> (a -> result) -> Table result
from table func =
    Table.from (getTable table) func


innerJoin : ComponentTable a -> Table (a -> result) -> Table result
innerJoin componentTable nextTable =
    Table.innerJoin (getTable componentTable) nextTable


toString : ComponentTable a -> String
toString (ComponentTable table ops) =
    "Table(\n"
        ++ Table.foldl
            (\entityId component accStr ->
                accStr
                    ++ "(EntityId="
                    ++ EntityId.toString entityId
                    ++ ", "
                    ++ ops.toString component
                    ++ "\n"
            )
            ""
            table
        ++ ")"


empty : ComponentOps a -> ComponentTable a
empty ops =
    ComponentTable Table.empty ops


get : EntityId -> ComponentTable a -> Maybe a
get entityId (ComponentTable table _) =
    Table.get entityId table


insert : EntityId -> a -> ComponentTable a -> ComponentTable a
insert entityId a (ComponentTable table ops) =
    ComponentTable (Table.insert entityId a table) ops


remove : EntityId -> ComponentTable a -> ComponentTable a
remove entityId (ComponentTable table ops) =
    ComponentTable (Table.remove entityId table) ops


mapRow : EntityId -> (a -> result) -> ComponentTable a -> Maybe result
mapRow entityId func table =
    Maybe.map func (get entityId table)


values : ComponentTable a -> List a
values (ComponentTable table _) =
    Table.values table


getTable : ComponentTable a -> Table a
getTable (ComponentTable table _) =
    table


getComponentOps : ComponentTable a -> ComponentOps a
getComponentOps (ComponentTable _ ops) =
    ops



-- Modifier


joinModifier :
    ( (ComponentTable a -> ComponentTable a) -> db -> db, b -> a )
    -> (b -> EntityId -> db -> db)
    -> b
    -> EntityId
    -> db
    -> db
joinModifier mapTable previousUpdater outputComponents entityId world =
    world
        |> previousUpdater outputComponents entityId
        |> updateComponent mapTable outputComponents entityId


updateComponent : ( (ComponentTable a -> ComponentTable a) -> db -> db, b -> a ) -> b -> EntityId -> db -> db
updateComponent ( mapTable, getA ) outputComponents entityId =
    mapTable (insert entityId (getA outputComponents))


mapEntitiesInTable : (EntityId -> a -> result) -> ComponentTable a -> EntitySet -> Table result
mapEntitiesInTable func table entitySet =
    Table.map func (filterEntitiesInTable entitySet (getTable table))


filterEntitiesInTable : EntitySet -> Table a -> Table a
filterEntitiesInTable (EntitySet _ entityList) table =
    Table.filter (\entityId _ -> List.member entityId entityList) table
