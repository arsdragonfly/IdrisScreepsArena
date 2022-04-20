module IdrisScreepsArena.FFI

import JS

export data Creep : Type where [external]

export data Flag : Type where [external]

export data ScreepsOK : Type where [external]

export
%foreign "javascript:lambda:() => OK"
ok : ScreepsOK

export
SafeCast ScreepsOK where
  safeCast v = if eqv v ok then Just ok else Nothing

export data ScreepsError : Type where [external]

export
%foreign "javascript:lambda:() => ERR_NOT_OWNER"
errNotOwner : ScreepsError

export
%foreign "javascript:lambda:() => ERR_NO_PATH"
errNoPath : ScreepsError

export
%foreign "javascript:lambda:() => ERR_NAME_EXISTS"
errNameExists : ScreepsError

export
%foreign "javascript:lambda:() => ERR_BUSY"
errBusy : ScreepsError

export
%foreign "javascript:lambda:() => ERR_NOT_FOUND"
errNotFound : ScreepsError

-- we ignore other aliases for this constant (ERR_NOT_ENOUGH_ENERGY and ERR_NOT_ENOUGH_EXTENSIONS)
export
%foreign "javascript:lambda:() => ERR_NOT_ENOUGH_RESOURCES"
errNotEnoughResources : ScreepsError

export
%foreign "javascript:lambda:() => ERR_INVALID_TARGET"
errInvalidTarget : ScreepsError

export
%foreign "javascript:lambda:() => ERR_FULL"
errFull : ScreepsError

export
%foreign "javascript:lambda:() => ERR_NOT_IN_RANGE"
errNotInRange : ScreepsError

export
%foreign "javascript:lambda:() => ERR_INVALID_ARGS"
errInvalidArgs : ScreepsError

export
%foreign "javascript:lambda:() => ERR_TIRED"
errTired : ScreepsError

export
%foreign "javascript:lambda:() => ERR_NO_BODYPART"
errNoBodypart : ScreepsError

export
SafeCast ScreepsError where
  safeCast v = choiceMap check screepsErrors where
    screepsErrors : List ScreepsError
    screepsErrors = [errNotOwner, errNoPath, errNameExists, errBusy, errNotFound, errNotEnoughResources, errInvalidTarget, errFull, errNotInRange, errInvalidArgs, errTired, errNoBodypart]
    check : (err : ScreepsError) -> Maybe ScreepsError
    check err = if eqv v err then Just err else Nothing

%foreign "javascript:lambda:(u, x) => x.x"
prim__x : forall a . a -> Int

%foreign "javascript:lambda:(u, x) => x.y"
prim__y : forall a . a -> Int

export
interface HasPosition a where
  posX : a -> Int
  posX = prim__x
  posY : a -> Int
  posY = prim__y

export
HasPosition Creep where

export
HasPosition Flag where

%foreign "javascript:lambda: () => getObjectsByPrototype(Creep)"
prim__getObjectsByPrototypeCreep : PrimIO (IArray Creep)

export
getObjectsByPrototypeCreep : (HasIO io) => io (IArray Creep)
getObjectsByPrototypeCreep = primIO prim__getObjectsByPrototypeCreep

%foreign "javascript:lambda: () => getObjectsByPrototype(Flag)"
prim__getObjectsByPrototypeFlag : PrimIO (IArray Flag)

export
getObjectsByPrototypeFlag : (HasIO io) => io (IArray Flag)
getObjectsByPrototypeFlag = primIO prim__getObjectsByPrototypeFlag

%foreign "javascript:lambda: () => getTicks()"
prim__getTicks : PrimIO Int

export
getTicks : (HasIO io) => io Int
getTicks = primIO prim__getTicks
