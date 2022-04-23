module IdrisScreepsArena.FFI

import JS
import JS.JSON
import Data.SOP
import Generics.Derive

%language ElabReflection

export
filterM : Applicative m => (a -> m Bool) -> List a -> m $ List a
filterM p = foldr (\x, acc => [|(\flg, l => ifThenElse flg (x :: l) l) (p x) acc|]) (pure [])

checkBy : (a -> b -> Bool) -> a -> b -> Maybe b
checkBy eq a target = if eq a target then Just target else Nothing

export data Creep : Type where [external]

export data Flag : Type where [external]

toJSONwithToFFI : {auto tf : ToFFI a b} -> {auto tj : ToJSON b} -> (a -> Value)
toJSONwithToFFI = toJSON@{tj} . toFFI@{tf}

fromJSONwithFromFFI : {auto ff : FromFFI a b} -> {auto fj : FromJSON b} -> Parser a
fromJSONwithFromFFI = \val => do
    bb <- fromJSON@{fj} val
    Just aa <- pure (fromFFI@{ff} bb)
      | Nothing => fail #"fromFFI failed for \#{jsShow bb}"#
    pure aa

export
(ToFFI a b, ToJSON b) => ToJSON a where
  toJSON = toJSONwithToFFI

export
(FromFFI a b, FromJSON b) => FromJSON a where
  fromJSON = fromJSONwithFromFFI

data PrimScreepsOK : Type where [external]

%foreign "javascript:lambda:() => OK"
prim__ok : PrimScreepsOK

SafeCast PrimScreepsOK where
  safeCast v = if eqv v prim__ok then Just prim__ok else Nothing

ToJSON PrimScreepsOK where
  toJSON = Num . believe_me

FromJSON PrimScreepsOK where
  fromJSON = withNumber "PrimScreepsOK" (\n => case safeCast n of
    Just v => pure v
    Nothing => fail #"cannot cast \#{jsShow n} to PrimScreepsOK"#)

public export
data ScreepsOK = OK

%runElab derive "ScreepsOK" [Generic, Meta, Eq, Ord, Show]

FromFFI ScreepsOK PrimScreepsOK where fromFFI _ = Just OK

ToFFI ScreepsOK PrimScreepsOK where toFFI _ = prim__ok

export
FromJSON ScreepsOK where
  fromJSON = fromJSONwithFromFFI

export
ToJSON ScreepsOK where
  toJSON = toJSONwithToFFI

data PrimScreepsError : Type where [external]

%foreign "javascript:lambda:() => ERR_NOT_OWNER"
prim__errNotOwner : PrimScreepsError

%foreign "javascript:lambda:() => ERR_NO_PATH"
prim__errNoPath : PrimScreepsError

%foreign "javascript:lambda:() => ERR_NAME_EXISTS"
prim__errNameExists : PrimScreepsError

%foreign "javascript:lambda:() => ERR_BUSY"
prim__errBusy : PrimScreepsError

%foreign "javascript:lambda:() => ERR_NOT_FOUND"
prim__errNotFound : PrimScreepsError

-- we ignore other aliases for this constant (ERR_NOT_ENOUGH_ENERGY and ERR_NOT_ENOUGH_EXTENSIONS)
%foreign "javascript:lambda:() => ERR_NOT_ENOUGH_RESOURCES"
prim__errNotEnoughResources : PrimScreepsError

%foreign "javascript:lambda:() => ERR_INVALID_TARGET"
prim__errInvalidTarget : PrimScreepsError

%foreign "javascript:lambda:() => ERR_FULL"
prim__errFull : PrimScreepsError

%foreign "javascript:lambda:() => ERR_NOT_IN_RANGE"
prim__errNotInRange : PrimScreepsError

%foreign "javascript:lambda:() => ERR_INVALID_ARGS"
prim__errInvalidArgs : PrimScreepsError

%foreign "javascript:lambda:() => ERR_TIRED"
prim__errTired : PrimScreepsError

%foreign "javascript:lambda:() => ERR_NO_BODYPART"
prim__errNoBodypart : PrimScreepsError

export
data ScreepsError = ErrNotOwner | ErrNoPath | ErrNameExists | ErrBusy | ErrNotFound | ErrNotEnoughResources | ErrInvalidTarget | ErrFull | ErrNotInRange | ErrInvalidArgs | ErrTired | ErrNoBodypart

%runElab derive "ScreepsError" [Generic, Meta, Eq, Ord, Show]

screepsErrors : List (ScreepsError, PrimScreepsError)
screepsErrors = [
  (ErrNotOwner, prim__errNotOwner),
  (ErrNoPath, prim__errNoPath),
  (ErrNameExists, prim__errNameExists),
  (ErrBusy, prim__errBusy),
  (ErrNotFound, prim__errNotFound),
  (ErrNotEnoughResources, prim__errNotEnoughResources),
  (ErrInvalidTarget, prim__errInvalidTarget),
  (ErrFull, prim__errFull),
  (ErrNotInRange, prim__errNotInRange),
  (ErrInvalidArgs, prim__errInvalidArgs),
  (ErrTired, prim__errTired),
  (ErrNoBodypart, prim__errNoBodypart)
  ]

SafeCast PrimScreepsError where
  safeCast v = choiceMap (checkBy eqv v) (map snd screepsErrors)

ToJSON PrimScreepsError where
  toJSON = Num . believe_me

FromJSON PrimScreepsError where
  fromJSON = withNumber "PrimScreepsError" (\n => case safeCast n of
    Just v => pure v
    Nothing => fail #"cannot cast \#{jsShow n} to PrimScreepsError"#)

FromFFI ScreepsError PrimScreepsError where
  fromFFI v = choiceMap check screepsErrors where
    check : (err : (ScreepsError, PrimScreepsError)) -> Maybe ScreepsError
    check (e, pe) = if eqv v pe then Just e else Nothing

-- TODO: get rid of the repetition here
ToFFI ScreepsError PrimScreepsError where
  toFFI ErrNotOwner = prim__errNotOwner
  toFFI ErrNoPath = prim__errNoPath
  toFFI ErrNameExists = prim__errNameExists
  toFFI ErrBusy = prim__errBusy
  toFFI ErrNotFound = prim__errNotFound
  toFFI ErrNotEnoughResources = prim__errNotEnoughResources
  toFFI ErrInvalidTarget = prim__errInvalidTarget
  toFFI ErrFull = prim__errFull
  toFFI ErrNotInRange = prim__errNotInRange
  toFFI ErrInvalidArgs = prim__errInvalidArgs
  toFFI ErrTired = prim__errTired
  toFFI ErrNoBodypart = prim__errNoBodypart

export
FromJSON ScreepsError where
  fromJSON = fromJSONwithFromFFI

export
ToJSON ScreepsError where
  toJSON = toJSONwithToFFI

-- if 'exists' is false (which happens for cached and newly created objects), x and y will be undefined
%foreign "javascript:lambda:(u, x) => x.x"
prim__x : forall a . a -> PrimIO (UndefOr Int32)

%foreign "javascript:lambda:(u, x) => x.y"
prim__y : forall a . a -> PrimIO (UndefOr Int32)

export
interface HasPosition a where
  posX : (HasIO io) => a -> io (Maybe Int32)
  posX v = map undeforToMaybe (primIO (prim__x v))
  posY : (HasIO io) => a -> io (Maybe Int32)
  posY v = map undeforToMaybe (primIO (prim__y v))

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
prim__getTicks : PrimIO Int32

export
getTicks : (HasIO io) => io Int32
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

transformReturnCode : PrimIO (Union2 PrimScreepsError PrimScreepsOK) -> JSIO (Maybe $ Either ScreepsError ScreepsOK)
transformReturnCode ret = do
  result <- primJS ret
  pure (map nsTo (fromFFI result))

%foreign "javascript:lambda:(u, creep, target) => creep.moveTo(target)"
prim__moveTo : forall a . Creep -> a -> PrimIO (Union2 PrimScreepsError PrimScreepsOK)

export
moveTo : (HasPosition o) => Creep -> o -> JSIO (Either ScreepsError ScreepsOK)
moveTo creep target = unMaybe "moveTo" $ transformReturnCode $ prim__moveTo creep target

%foreign "javascript:lambda:(u, creep, target) => creep.attack(target)"
prim__attack : forall a . Creep -> a -> PrimIO (Union2 PrimScreepsError PrimScreepsOK)

export
interface Attackable a where

export
Attackable Creep where

export
attack : (Attackable o) => Creep -> o -> JSIO (Either ScreepsError ScreepsOK)
attack creep target = unMaybe "attack" $ transformReturnCode $ prim__attack creep target

data PrimBodypart : Type where [external]

%foreign "javascript:lambda:() => MOVE"
prim__bodypartMove : PrimBodypart

%foreign "javascript:lambda:() => WORK"
prim__bodypartWork : PrimBodypart

%foreign "javascript:lambda:() => CARRY"
prim__bodypartCarry : PrimBodypart

%foreign "javascript:lambda:() => ATTACK"
prim__bodypartAttack : PrimBodypart

%foreign "javascript:lambda:() => RANGED_ATTACK"
prim__bodypartRangedAttack : PrimBodypart

%foreign "javascript:lambda:() => HEAL"
prim__bodypartHeal : PrimBodypart

%foreign "javascript:lambda:() => TOUGH"
prim__bodypartTough : PrimBodypart

public export
data Bodypart = Move | Work | Carry | Attack | RangedAttack | Heal | Tough

%runElab derive "Bodypart" [Generic, Meta, Eq, Ord, Show]

screepsBodyparts : List (Bodypart, PrimBodypart)
screepsBodyparts = [
  (Move, prim__bodypartMove),
  (Work, prim__bodypartWork),
  (Carry, prim__bodypartCarry),
  (Attack, prim__bodypartAttack),
  (RangedAttack, prim__bodypartRangedAttack),
  (Heal, prim__bodypartHeal),
  (Tough, prim__bodypartTough)
  ]

SafeCast PrimBodypart where
  safeCast v = choiceMap (checkBy eqv v) (map snd screepsBodyparts)

ToJSON PrimBodypart where
  toJSON = Str . believe_me

FromJSON PrimBodypart where
  fromJSON = withString "PrimBodypart" (\n => case safeCast n of
    Just v => pure v
    Nothing => fail #"cannot cast \#{jsShow n} to PrimBodypart"#)

FromFFI Bodypart PrimBodypart where
  fromFFI v = choiceMap check screepsBodyparts where
    check : (err : (Bodypart, PrimBodypart)) -> Maybe Bodypart
    check (e, pe) = if eqv v pe then Just e else Nothing

ToFFI Bodypart PrimBodypart where
  toFFI Move = prim__bodypartMove
  toFFI Work = prim__bodypartWork
  toFFI Carry = prim__bodypartCarry
  toFFI Attack = prim__bodypartAttack
  toFFI RangedAttack = prim__bodypartRangedAttack
  toFFI Heal = prim__bodypartHeal
  toFFI Tough = prim__bodypartTough

export
FromJSON Bodypart where
  fromJSON = fromJSONwithFromFFI

export
ToJSON Bodypart where
  toJSON = toJSONwithToFFI

public export 
record BodypartMounted where
  constructor MkBodypartMounted
  type : String
  hits : Int32

%runElab derive "BodypartMounted" [Generic, Meta, Eq, Ord, Show, FromJSON1, ToJSON1]
