module IdrisScreepsArena.FFI

import JS
import Data.SOP
import Generics.Derive

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
prim__x : forall a . a -> PrimIO Int

%foreign "javascript:lambda:(u, x) => x.y"
prim__y : forall a . a -> PrimIO Int

export
interface HasPosition a where
  posX : (HasIO io) => a -> io Int
  posX v = primIO (prim__x v)
  posY : (HasIO io) => a -> io Int
  posY v = primIO (prim__y v)

export
HasPosition Creep where

export
HasPosition Flag where

%foreign "javascript:lambda:(u, x) => x.my"
prim__my : forall a . a -> PrimIO Boolean

export
interface Owned a where
  my : a -> JSIO Bool
  my v = tryJS "my" $ prim__my v

export
Owned Creep where

%foreign "javascript:lambda: () => getObjectsByPrototype(Creep)"
prim__getObjectsByPrototypeCreep : PrimIO (IArray Creep)

export
getObjectsByPrototypeCreep : (HasIO io) => io (List Creep)
getObjectsByPrototypeCreep = map arrayToList $ primIO prim__getObjectsByPrototypeCreep

%foreign "javascript:lambda: () => getObjectsByPrototype(Flag)"
prim__getObjectsByPrototypeFlag : PrimIO (IArray Flag)

export
getObjectsByPrototypeFlag : (HasIO io) => io (List Flag)
getObjectsByPrototypeFlag = map arrayToList $ primIO prim__getObjectsByPrototypeFlag

%foreign "javascript:lambda: () => getTicks()"
prim__getTicks : PrimIO Int

export
getTicks : (HasIO io) => io Int
getTicks = primIO prim__getTicks

-- courtesy of stefan-hoeck
data SingletonsOf : (ks : List k) -> (kss : List (List k)) -> Type where
  [search kss]
  SNil : SingletonsOf [] []
  SCons : SingletonsOf ks kss -> SingletonsOf (k :: ks) ([k] :: kss)

nsToSOP : NS_ k f ks -> (prf : SingletonsOf ks kss) => SOP_ k f kss
nsToSOP (Z v) {prf = SCons so} = MkSOP (Z [v])
nsToSOP (S x) {prf = SCons so} = let MkSOP v = nsToSOP x in MkSOP (S v)

export
nsTo :  Generic t code
     => SingletonsOf ts code
     => NS I ts
     -> t
nsTo ns = to $ nsToSOP ns

nsFromSOP : SOP_ k f kss -> (prf : SingletonsOf ks kss) => NS_ k f ks
nsFromSOP (MkSOP $ Z [v]) {prf = SCons so} = Z v
nsFromSOP (MkSOP $ S x)   {prf = SCons so} = S $ nsFromSOP (MkSOP x)

export
nsFrom :  Generic t code
       => SingletonsOf ts code
       => t
       -> NS I ts
nsFrom v = nsFromSOP $ from v

transformReturnCode : PrimIO (Union2 ScreepsError ScreepsOK) -> JSIO (Maybe $ Either ScreepsError ScreepsOK)
transformReturnCode ret = do
  result <- primJS ret
  pure (map nsTo (fromUnion2 result))

%foreign "javascript:lambda:(u, creep, target) => creep.moveTo(target)"
prim__moveTo : forall a . Creep -> a -> PrimIO (Union2 ScreepsError ScreepsOK)

export
moveTo : (HasPosition o) => Creep -> o -> JSIO (Either ScreepsError ScreepsOK)
moveTo creep target = unMaybe "moveTo" $ transformReturnCode $ prim__moveTo creep target

%foreign "javascript:lambda:(u, creep, target) => creep.attack(target)"
prim__attack : forall a . Creep -> a -> PrimIO (Union2 ScreepsError ScreepsOK)

export
interface Attackable a where

export
Attackable Creep where

export
attack : (Attackable o) => Creep -> o -> JSIO (Either ScreepsError ScreepsOK)
attack creep target = unMaybe "attack" $ transformReturnCode $ prim__attack creep target

