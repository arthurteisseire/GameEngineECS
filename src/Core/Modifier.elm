module Core.Modifier exposing (..)

import Core.EntityId exposing (EntityId)


type alias SimpleModifier a b =
    { get : b -> a
    , set : a -> b -> b
    }


type alias Modifier a b =
    { get : b -> a
    , set : a -> b -> b
    , map : (a -> a) -> b -> b
    }


init : SimpleModifier a b -> Modifier a b
init modifier =
    { get = modifier.get
    , set = modifier.set
    , map = \func b -> modifier.set (func (modifier.get b)) b
    }


select : b -> EntityId -> db -> db
select _ _ db =
    db
