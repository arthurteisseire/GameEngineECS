module Core.EntitySet exposing (..)

import Core.EntityId as EntityId exposing (EntityId)


type EntitySet
    = EntitySet Int (List EntityId)


empty : EntitySet
empty =
    EntitySet 0 []


isEmpty : EntitySet -> Bool
isEmpty entitySet =
    size entitySet == 0


size : EntitySet -> Int
size (EntitySet nextId _) =
    nextId - 1


addEntity : EntitySet -> ( EntitySet, EntityId )
addEntity (EntitySet nextId entities) =
    ( EntitySet (nextId + 1) (EntityId.fromInt nextId :: entities)
    , EntityId.fromInt nextId
    )


remove : EntityId -> EntitySet -> EntitySet
remove entityId (EntitySet lastId list) =
    EntitySet lastId (List.filter ((/=) entityId) list)


member : EntityId -> EntitySet -> Bool
member entityId (EntitySet _ list) =
    List.member entityId list


map : (EntityId -> result) -> EntitySet -> List result
map func entitySet =
    foldl
        (\entityId acc -> func entityId :: acc)
        []
        entitySet


foldl : (EntityId -> result -> result) -> result -> EntitySet -> result
foldl func result (EntitySet _ list) =
    List.foldl
        (\entityId acc -> func entityId acc)
        result
        list


removeIf : Bool -> EntityId -> EntitySet -> EntitySet
removeIf isBad entityId entitySet =
    if isBad then
        remove entityId entitySet

    else
        entitySet


filter : (EntityId -> Bool) -> EntitySet -> EntitySet
filter isGood (EntitySet lastId list) =
    EntitySet lastId (List.filter isGood list)


addNEntities : Int -> EntitySet -> ( EntitySet, List EntityId )
addNEntities n entitySet =
    let
        func : Int -> EntitySet -> List EntityId -> ( EntitySet, List EntityId )
        func currentN currentEntitySet currentEntityIds =
            case currentN of
                0 ->
                    ( currentEntitySet, currentEntityIds )

                _ ->
                    let
                        ( newSet, entityId ) =
                            addEntity currentEntitySet
                    in
                    func (currentN - 1) newSet (entityId :: currentEntityIds)
    in
    func n entitySet []
