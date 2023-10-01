module Core.Context exposing (..)

import Core.ComponentTable as ComponentTable exposing (ComponentTable)
import Core.EntityId exposing (EntityId)
import Core.EntitySet as EntitySet exposing (EntitySet(..))
import Core.Table as Table exposing (Table)
import Html exposing (Html)


type alias ContextSystem context msg =
    { update : msg -> context -> ( context, Cmd msg )
    , view : context -> Html msg
    , subscriptions : context -> Sub msg
    }


type alias ContextOperations context =
    { toStrings : EntityId -> context -> List String
    , remove : EntityId -> context -> context
    }



-- Update entities


mapComponentsFull :
    { func : inputs -> result
    , inputComponents : EntityId -> context -> inputs
    }
    -> EntityId
    -> context
    -> result
mapComponentsFull { func, inputComponents } entityId context =
    func (inputComponents entityId context)


mapComponents :
    { func : inputs -> result
    , inputComponents : EntityId -> context -> Maybe inputs
    }
    -> EntityId
    -> context
    -> Maybe result
mapComponents { func, inputComponents } entityId context =
    Maybe.map
        func
        (inputComponents entityId context)


updateComponents :
    { func : inputs -> outputs
    , inputComponents : EntityId -> context -> Maybe inputs
    , output : outputs -> EntityId -> context -> context
    }
    -> EntityId
    -> context
    -> context
updateComponents { func, inputComponents, output } entityId context =
    mapComponents
        { func = \components -> output (func components) entityId context
        , inputComponents = inputComponents
        }
        entityId
        context
        |> Maybe.withDefault context


updateComponentsWithOthers :
    { func : Table others -> inputs -> outputs
    , inputComponents : EntityId -> context -> Maybe inputs
    , otherComponents : context -> Table others
    , output : outputs -> EntityId -> context -> context
    }
    -> EntityId
    -> context
    -> context
updateComponentsWithOthers { func, inputComponents, otherComponents, output } entityId context =
    updateComponents
        { func = func (otherComponents context |> Table.remove entityId)
        , inputComponents = inputComponents
        , output = output
        }
        entityId
        context



-- Table relations


select : a -> a
select =
    Table.select


fromEntities : (context -> EntitySet) -> a -> context -> Table a
fromEntities getEntitySet a context =
    EntitySet.foldl
        (\entity table -> Table.insert entity a table)
        Table.empty
        (getEntitySet context)


from : (context -> Table a) -> (a -> result) -> context -> Table result
from getTable func context =
    Table.from (getTable context) func


innerJoin : (context -> ComponentTable a) -> (context -> Table (a -> result)) -> context -> Table result
innerJoin getTable getNextTable context =
    ComponentTable.innerJoin (getTable context) (getNextTable context)
