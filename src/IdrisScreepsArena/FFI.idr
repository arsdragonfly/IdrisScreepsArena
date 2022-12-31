module IdrisScreepsArena.FFI

import JS
import JSON.Derive
import Data.SOP
import Generics.Derive

%default total
%language ElabReflection

%foreign "javascript:lambda:(u,o) => JSON.stringify(o)"
prim__stringify : forall a . a -> String

export
exists : (Type -> Type) -> Type
exists c = {b : Type} -> ({a : Type} -> {prf : c a} -> a -> b) -> b

export
packExists : {c : Type -> Type} -> {a : Type} -> {auto prf : c a} -> a -> exists c
packExists x = \f => f {a} {prf} x

export
unpackExists : {c : Type -> Type} -> exists c -> {b : Type} -> ({a : Type} -> {prf : c a} -> a -> b) -> b
unpackExists exT = exT

public export
interface ToFFIExists a where
  constructor MkToFFIExists
  ToFFITarget : Type
  toFFIImplementation: ToFFI a ToFFITarget

public export
interface (ToFFIExists a) => ToFFISatisfiesConstraint (0 c: Type -> Type) a where
  constructor MkToFFISatisfiesConstraint
  toFFITargetConstraintEvidence : c (ToFFITarget {a})

export
%hint
withToFFIConstraint : (impl: ToFFIExists a) -> c (ToFFITarget@{impl}) -> ToFFISatisfiesConstraint c a
withToFFIConstraint impl cb = MkToFFISatisfiesConstraint cb

export
%hint
toFFIConstraintCovariant : (impl: ToFFISatisfiesConstraint c a) -> (cov: forall b . c b -> d b) -> ToFFISatisfiesConstraint d a
toFFIConstraintCovariant impl cov = MkToFFISatisfiesConstraint (cov (toFFITargetConstraintEvidence@{impl}))

export
filterM : Applicative m => (a -> m Bool) -> List a -> m $ List a
filterM p = foldr (\x, acc => [|(\flg, l => ifThenElse flg (x :: l) l) (p x) acc|]) (pure [])

export
checkBy : (a -> b -> Bool) -> a -> b -> Maybe b
checkBy eq a target = if eq a target then Just target else Nothing

public export
%hint
toFFIExistsInt32 : {auto prf: ToFFI a Int32} -> ToFFIExists a
toFFIExistsInt32 = MkToFFIExists Int32 prf

export
data Creep : Type where [external]

export
ToFFI Creep Creep where toFFI = id

public export
%hint
toFFIExistsCreep : {auto prf: ToFFI a Creep} -> ToFFIExists a
toFFIExistsCreep = MkToFFIExists Creep prf

export
FromFFI Creep Creep where fromFFI = Just

export
data Flag : Type where [external]

export
ToFFI Flag Flag where toFFI = id

public export
%hint
toFFIExistsFlag : {auto prf: ToFFI a Flag} -> ToFFIExists a
toFFIExistsFlag = MkToFFIExists Flag prf

export
FromFFI Flag Flag where fromFFI = Just

export
data Tower : Type where [external]

export
ToFFI Tower Tower where toFFI = id

public export
%hint
toFFIExistsTower : {auto prf: ToFFI a Tower} -> ToFFIExists a
toFFIExistsTower = MkToFFIExists Tower prf

export
FromFFI Tower Tower where fromFFI = Just

export
data Container : Type where [external]

export
ToFFI Container Container where toFFI = id

public export
%hint
toFFIExistsContainer : {auto prf: ToFFI a Container} -> ToFFIExists a
toFFIExistsContainer = MkToFFIExists Container prf

export
FromFFI Container Container where fromFFI = Just

toJSONwithToFFI : Encoder v => {auto tf : ToFFI a b} -> {auto tj : ToJSON b} -> (a -> v)
toJSONwithToFFI = toJSON@{tj} . toFFI@{tf}

fromJSONwithFromFFI : forall v, obj . Value v obj => {auto ff : FromFFI a b} -> {auto fj : FromJSON b} -> Parser v a
fromJSONwithFromFFI = \val => do
    bb <- fromJSON@{fj} val
    Just aa <- pure (fromFFI@{ff} bb)
      | Nothing => fail #"fromFFI failed for \#{jsShow bb}"#
    pure aa

-- TODO: this isn't working; change this to %hint with MkToJSON and MkFromJSON

-- export
-- (ToFFI a b, ToJSON b) => ToJSON a where
--   toJSON = toJSONwithToFFI

-- export
-- (FromFFI a b, FromJSON b) => FromJSON a where
--   fromJSON = fromJSONwithFromFFI

data PrimScreepsOK : Type where [external]

%foreign "javascript:lambda:() => OK"
prim__ok : PrimScreepsOK

SafeCast PrimScreepsOK where
  safeCast v = if eqv v prim__ok then Just prim__ok else Nothing

ToJSON PrimScreepsOK where
  toJSON = smallInteger . believe_me

FromJSON PrimScreepsOK where
  fromJSON = withNumber "PrimScreepsOK" (\n => case safeCast n of
    Just v => pure v
    Nothing => fail #"cannot cast \#{jsShow n} to PrimScreepsOK"#)

public export
data ScreepsOK = OK

%runElab derive "ScreepsOK" [Generic, Meta, Derive.Eq.Eq, Derive.Ord.Ord, Derive.Show.Show]

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

%runElab derive "ScreepsError" [Generic, Meta, Derive.Eq.Eq, Derive.Ord.Ord, Derive.Show.Show]

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
  toJSON = smallInteger . believe_me

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


-- TODO: complete this
public export
data PrimGameObject : Type -> Type where
  PrimGameObjectCreep : PrimGameObject Creep
  PrimGameObjectFlag : PrimGameObject Flag
  PrimGameObjectTower : PrimGameObject Tower
  PrimGameObjectContainer : PrimGameObject Container

%foreign "javascript:lambda:(u, x) => x.exists"
prim__exists : forall a. a -> PrimIO Boolean

export
interface ToFFISatisfiesConstraint PrimGameObject a => GameObject a where

export
ToFFISatisfiesConstraint PrimGameObject a => GameObject a where

export
gameObjectExists : (GameObject a) => a -> JSIO Bool
gameObjectExists v = tryJS "gameObjectExists" $ prim__exists $ toFFI@{toFFIImplementation} v

-- if 'exists' is false (which happens for cached and newly created objects), x and y will be undefined
%foreign "javascript:lambda:(u, x) => x.x"
prim__x : forall a . a -> PrimIO (UndefOr Int32)

%foreign "javascript:lambda:(u, x) => x.y"
prim__y : forall a . a -> PrimIO (UndefOr Int32)

-- TODO: support objects with x and y fields
export
interface PrimHasPosition a where
  posX' : (HasIO io) => a -> io (Maybe Int32)
  posX' v = map undeforToMaybe $ primIO $ prim__x $ v
  posY' : (HasIO io) => a -> io (Maybe Int32)
  posY' v = map undeforToMaybe $ primIO $ prim__x $ v

export
PrimGameObject a => PrimHasPosition a where

export
interface ToFFISatisfiesConstraint PrimHasPosition a => HasPosition a where

export
ToFFISatisfiesConstraint PrimHasPosition a => HasPosition a where

export
posX : (HasIO io) => (HasPosition a) => a -> io (Maybe Int32)
posX v = posX'@{toFFITargetConstraintEvidence {a=a}} $ toFFI@{toFFIImplementation} v

export
posY : (HasIO io) => (HasPosition a) => a -> io (Maybe Int32)
posY v = posY'@{toFFITargetConstraintEvidence {a=a}} $ toFFI@{toFFIImplementation} v

export
data PrimStructure : Type -> Type where
  PrimStructureTower : PrimStructure Tower
  PrimStructureContainer : PrimStructure Container

export
%hint
primStructureToPrimGameObject : PrimStructure a -> PrimGameObject a
primStructureToPrimGameObject PrimStructureTower = PrimGameObjectTower
primStructureToPrimGameObject PrimStructureContainer = PrimGameObjectContainer

public export
data PrimOwned : Type -> Type where
  PrimOwnedCreep : PrimOwned Creep
  PrimOwnedTower : PrimOwned Tower

export
%hint
primOwnedToPrimGameObject : PrimOwned a -> PrimGameObject a
primOwnedToPrimGameObject PrimOwnedCreep = PrimGameObjectCreep
primOwnedToPrimGameObject PrimOwnedTower = PrimGameObjectTower

%foreign "javascript:lambda:(u, x) => x.my"
prim__my : forall a . a -> PrimIO Boolean

export
interface ToFFISatisfiesConstraint PrimOwned a => Owned a where

export
ToFFISatisfiesConstraint PrimOwned a => Owned a where

export
my : (Owned a) => a -> JSIO Bool
my v = tryJS "my" $ prim__my $ toFFI@{toFFIImplementation} v

%foreign "javascript:lambda: () => getObjectsByPrototype(Creep)"
prim__getObjectsByPrototypeCreep : PrimIO (IArray Creep)

%foreign "javascript:lambda: () => getObjectsByPrototype(Flag)"
prim__getObjectsByPrototypeFlag : PrimIO (IArray Flag)

%foreign "javascript:lambda: () => getObjectsByPrototype(StructureTower)"
prim__getObjectsByPrototypeTower : PrimIO (IArray Tower)

%foreign "javascript:lambda: () => getObjectsByPrototype(StructureContainer)"
prim__getObjectsByPrototypeContainer : PrimIO (IArray Container)

getObjectsPrim : PrimGameObject a -> PrimIO (IArray a)
getObjectsPrim PrimGameObjectCreep = prim__getObjectsByPrototypeCreep
getObjectsPrim PrimGameObjectFlag = prim__getObjectsByPrototypeFlag
getObjectsPrim PrimGameObjectTower = prim__getObjectsByPrototypeTower
getObjectsPrim PrimGameObjectContainer = prim__getObjectsByPrototypeContainer

export
getObjects : (HasIO io) => (a: Type) -> {auto prf : PrimGameObject a} -> io (List a)
getObjects _ {prf} = map arrayToList $ primIO $ getObjectsPrim prf

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

0 PrimAction2 : Type
PrimAction2 = forall a, b . a -> b -> PrimIO (Union2 PrimScreepsError PrimScreepsOK)

action2 : (ToFFIExists a) => (ToFFIExists b) => String -> PrimAction2 -> a -> b -> JSIO (Either ScreepsError ())
action2 actionName primAction aa bb = let
  aaToFFI = toFFI@{toFFIImplementation} aa
  bbToFFI = toFFI@{toFFIImplementation} bb in
  (map ignore) $ unMaybe actionName $ transformReturnCode $ primAction aaToFFI bbToFFI

0 PrimAction3 : Type
PrimAction3 = forall a, b, c . a -> b -> c -> PrimIO (Union2 PrimScreepsError PrimScreepsOK)

action3 : (ToFFIExists a) => (ToFFIExists b) => (ToFFIExists c) => String -> PrimAction3 -> a -> b -> c -> JSIO (Either ScreepsError ())
action3 actionName primAction aa bb cc = let
  aaToFFI = toFFI@{toFFIImplementation} aa
  bbToFFI = toFFI@{toFFIImplementation} bb
  ccToFFI = toFFI@{toFFIImplementation} cc in
  (map ignore) $ unMaybe actionName $ transformReturnCode $ primAction aaToFFI bbToFFI ccToFFI

0 PrimAction4 : Type
PrimAction4 = forall a, b, c, d . a -> b -> c -> d -> PrimIO (Union2 PrimScreepsError PrimScreepsOK)

action4 : (ToFFIExists a) => (ToFFIExists b) => (ToFFIExists c) => (ToFFIExists d) => String -> PrimAction4 -> a -> b -> c -> d -> JSIO (Either ScreepsError ())
action4 actionName primAction aa bb cc dd = let
  aaToFFI = toFFI@{toFFIImplementation} aa
  bbToFFI = toFFI@{toFFIImplementation} bb
  ccToFFI = toFFI@{toFFIImplementation} cc
  ddToFFI = toFFI@{toFFIImplementation} dd in
  (map ignore) $ unMaybe actionName $ transformReturnCode $ primAction aaToFFI bbToFFI ccToFFI ddToFFI

%foreign "javascript:lambda:(t, c, target, creep) => creep.moveTo(target)"
prim__moveTo : PrimAction2

export
moveTo : (ToFFI c Creep) => (HasPosition o) => o -> c -> JSIO (Either ScreepsError ())
moveTo = action2 "moveTo" prim__moveTo

%foreign "javascript:lambda:(u, x) => x.hits"
prim__hits : forall a . a -> PrimIO Int32

%foreign "javascript:lambda:(u, x) => x.hitsMax"
prim__hitsMax : forall a . a -> PrimIO Int32

%foreign "javascript:lambda:(u, x) => x.hits < x.hitsMax"
prim__wounded : forall a . a -> PrimIO Boolean

public export
data PrimAttackable : Type -> Type where
  PrimAttackableCreep : PrimAttackable Creep
  PrimAttackableTower : PrimAttackable Tower
  PrimAttackableContainer : PrimAttackable Container

export
%hint
primAttackableToPrimGameObject : PrimAttackable a -> PrimGameObject a
primAttackableToPrimGameObject PrimAttackableCreep = PrimGameObjectCreep
primAttackableToPrimGameObject PrimAttackableTower = PrimGameObjectTower
primAttackableToPrimGameObject PrimAttackableContainer = PrimGameObjectContainer

export
%hint
primStructureToPrimAttackable : PrimStructure a -> PrimAttackable a
primStructureToPrimAttackable PrimStructureTower = PrimAttackableTower
primStructureToPrimAttackable PrimStructureContainer = PrimAttackableContainer

export
interface ToFFISatisfiesConstraint PrimAttackable a => Attackable a where

export
ToFFISatisfiesConstraint PrimAttackable a => Attackable a where

export
hits : (HasIO io) => (Attackable a) => a -> io Int32
hits v = primIO $ prim__hits $ toFFI@{toFFIImplementation} v

export
hitsMax : (HasIO io) => (Attackable a) => a -> io Int32
hitsMax v = primIO $ prim__hitsMax $ toFFI@{toFFIImplementation} v

export
wounded : (Attackable a) => a -> JSIO Bool
wounded v = tryJS "wounded" $ prim__wounded $ toFFI@{toFFIImplementation} v

%foreign "javascript:lambda:(u, v, target, a) => a.attack(target)"
prim__attack : PrimAction2

%foreign "javascript:lambda:(u, v, target, a) => a.rangedAttack(target)"
prim__rangedAttack : PrimAction2

%foreign "javascript:lambda:(u, v, target, a) => a.rangedMassAttack(target)"
prim__rangedMassAttack : PrimAction2

public export
data PrimAttacker : Type -> Type where
  PrimAttackerCreep : PrimAttacker Creep
  PrimAttackerTower : PrimAttacker Tower

export
%hint
primAttackerToPrimGameObject : PrimAttacker a -> PrimGameObject a
primAttackerToPrimGameObject PrimAttackerCreep = PrimGameObjectCreep
primAttackerToPrimGameObject PrimAttackerTower = PrimGameObjectTower

export
interface ToFFISatisfiesConstraint PrimAttacker a => Attacker a where

export
ToFFISatisfiesConstraint PrimAttacker a => Attacker a where

export
attack : (Attacker a) => (Attackable o) => o -> a -> JSIO (Either ScreepsError ())
attack = action2 "attack" prim__attack

export
%hint
primAttackerIsEitherCreepOrTower : (val: PrimAttacker a) -> Either (Equal val PrimAttackerCreep) (Equal val PrimAttackerTower)
primAttackerIsEitherCreepOrTower v = case v of
  PrimAttackerCreep => Left Refl
  PrimAttackerTower => Right Refl

-- convenience overloads so that tower's rangedAttack and rangedMassAttack are all attack under the hood
export
rangedAttack : (Attacker a) => (Attackable o) => o -> a -> JSIO (Either ScreepsError ())
rangedAttack = case primAttackerIsEitherCreepOrTower (toFFITargetConstraintEvidence {a=a}) of
  Left _ => action2 "rangedAttack" prim__rangedAttack
  Right _ => action2 "attack" prim__attack

export
rangedMassAttack : (Attacker a) => (Attackable o) => o -> a -> JSIO (Either ScreepsError ())
rangedMassAttack = case primAttackerIsEitherCreepOrTower (toFFITargetConstraintEvidence {a=a}) of
  Left _ => action2 "rangedMassAttack" prim__rangedMassAttack
  Right _ => action2 "attack" prim__attack
    
%foreign "javascript:lambda:(u, v, target, a) => a.heal(target)"
prim__heal : PrimAction2

%foreign "javascript:lambda:(u, v, target, a) => a.rangedHeal(target)"
prim__rangedHeal : PrimAction2

public export
data PrimHealer : Type -> Type where
  PrimHealerCreep : PrimHealer Creep
  PrimHealerTower : PrimHealer Tower

export
%hint
primHealerToPrimGameObject : PrimHealer a -> PrimGameObject a
primHealerToPrimGameObject PrimHealerCreep = PrimGameObjectCreep
primHealerToPrimGameObject PrimHealerTower = PrimGameObjectTower

export
interface ToFFISatisfiesConstraint PrimHealer a => Healer a where

export
ToFFISatisfiesConstraint PrimHealer a => Healer a where

export
heal : (Healer a) => (ToFFI o Creep) => o -> a -> JSIO (Either ScreepsError ())
heal = action2 "heal" prim__heal

export
%hint
primHealerIsEitherCreepOrTower : (val: PrimHealer a) -> Either (Equal val PrimHealerCreep) (Equal val PrimHealerTower)
primHealerIsEitherCreepOrTower v = case v of
  PrimHealerCreep => Left Refl
  PrimHealerTower => Right Refl

-- convenience overload so that tower's rangedHeal is heal under the hood
rangedHeal : (Healer a) => (ToFFI o Creep) => o -> a -> JSIO (Either ScreepsError ())
rangedHeal = case primHealerIsEitherCreepOrTower (toFFITargetConstraintEvidence {a=a}) of
  Left _ => action2 "rangedHeal" prim__rangedHeal
  Right _ => action2 "heal" prim__heal

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

%runElab derive "Bodypart" [Generic, Meta, Derive.Eq.Eq, Derive.Ord.Ord, Derive.Show.Show]

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
  toJSON = string . believe_me

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

public export
%hint
toFFIExistsPrimBodypart : {auto prf: ToFFI a PrimBodypart} -> ToFFIExists a
toFFIExistsPrimBodypart = MkToFFIExists PrimBodypart prf

export
FromJSON Bodypart where
  fromJSON = fromJSONwithFromFFI

export
ToJSON Bodypart where
  toJSON = toJSONwithToFFI

public export 
record BodypartMounted where
  constructor MkBodypartMounted
  type : Bodypart
  hits : Int32

%runElab derive "BodypartMounted" [Generic, Meta, Derive.Eq.Eq, Derive.Ord.Ord, Derive.Show.Show, FromJSON, ToJSON]

%foreign "javascript:lambda:(c, creep) => creep.body"
prim__body : forall a . a -> PrimIO (IArray IObject)

export
body : (ToFFI c Creep) => c -> JSIO (List BodypartMounted)
body creep = do
  objs <- primJS $ prim__body (toFFI creep)
  case decodeEither (prim__stringify objs) of
    Right result => pure result
    Left jsonErr => throwError $ Caught #"JSON decode failed for List BodypartMounted: \#{jsShow jsonErr}"#

data PrimResource : Type where [external]

%foreign "javascript:lambda:() => RESOURCE_ENERGY"
prim__resourceEnergy : PrimResource

public export
data Resource = Energy

%runElab derive "Resource" [Generic, Meta, Derive.Eq.Eq, Derive.Ord.Ord, Derive.Show.Show]

screepsResources : List (Resource, PrimResource)
screepsResources = [
  (Energy, prim__resourceEnergy)
  ]

SafeCast PrimResource where
  safeCast v = choiceMap (checkBy eqv v) (map snd screepsResources)

ToJSON PrimResource where
  toJSON = string . believe_me

FromJSON PrimResource where
  fromJSON = withString "PrimResource" (\n => case safeCast n of
    Just v => pure v
    Nothing => fail #"cannot cast \#{jsShow n} to PrimResource"#)

FromFFI Resource PrimResource where
  fromFFI v = choiceMap check screepsResources where
    check : (err : (Resource, PrimResource)) -> Maybe Resource
    check (e, pe) = if eqv v pe then Just e else Nothing

ToFFI Resource PrimResource where
  toFFI Energy = prim__resourceEnergy

public export
%hint
toFFIExistsPrimResource : {auto prf: ToFFI a PrimResource} -> ToFFIExists a
toFFIExistsPrimResource = MkToFFIExists PrimResource prf

public export
data PrimStore : Type -> Type where
  PrimStoreCreep : PrimStore Creep
  PrimStoreTower : PrimStore Tower
  PrimStoreContainer : PrimStore Container

export
%hint
primStoreToPrimGameObject : PrimStore a -> PrimGameObject a
primStoreToPrimGameObject PrimStoreCreep = PrimGameObjectCreep
primStoreToPrimGameObject PrimStoreTower = PrimGameObjectTower
primStoreToPrimGameObject PrimStoreContainer = PrimGameObjectContainer

public export
data PrimMayHoldResource : (r : Resource) -> (a : Type) -> Type where
  [search r, search a]
  PrimMayHoldResourceEnergyCreep : PrimMayHoldResource Energy Creep
  PrimMayHoldResourceEnergyTower : PrimMayHoldResource Energy Tower
  PrimMayHoldResourceEnergyContainer : PrimMayHoldResource Energy Container

export
%hint
primMayHoldResourceToPrimStore : PrimMayHoldResource r a -> PrimStore a
primMayHoldResourceToPrimStore PrimMayHoldResourceEnergyCreep = PrimStoreCreep
primMayHoldResourceToPrimStore PrimMayHoldResourceEnergyTower = PrimStoreTower
primMayHoldResourceToPrimStore PrimMayHoldResourceEnergyContainer = PrimStoreContainer

export
interface ToFFISatisfiesConstraint PrimStore a => Store a where

export
ToFFISatisfiesConstraint PrimStore a => Store a where

%foreign "javascript:lambda:(u, v, w, resource, target, creep) => creep.transfer(target, resource)"
prim__transferAll : PrimAction3

%foreign "javascript:lambda:(u, v, w, x, amount, resource, target, creep) => creep.transfer(target, resource, amount)"
prim__transfer : PrimAction4

%foreign "javascript:lambda:(u, v, w, resource, target, creep) => creep.withdraw(target, resource)"
prim__withdrawAll : PrimAction3

%foreign "javascript:lambda:(u, v, w, x, amount, resource, target, creep) => creep.withdraw(target, resource, amount)"
prim__withdraw : PrimAction4

%foreign "javascript:lambda:(u, v, r, a) => a.store.getCapacity(r)"
prim__getCapacity : forall a, b . a -> b -> PrimIO Int32

%foreign "javascript:lambda:(u, v, r, a) => a.store.getFreeCapacity(r)"
prim__getFreeCapacity: forall a, b . a -> b -> PrimIO Int32

%foreign "javascript:lambda:(u, v, r, a) => a.store.getUsedCapacity(r)"
prim__getUsedCapacity: forall a, b . a -> b -> PrimIO Int32

export
interface ToFFISatisfiesConstraint (PrimMayHoldResource r) a => MayHoldResource r a where

export
ToFFISatisfiesConstraint (PrimMayHoldResource r) a => MayHoldResource r a where

export
transferAll : (ToFFI c Creep) => (r : Resource) -> (MayHoldResource r a) => a -> c -> JSIO (Either ScreepsError ())
transferAll r a c = action3 "transferAll" prim__transferAll r a c

export
transfer : (ToFFI c Creep) => Int32 -> (r : Resource) -> (MayHoldResource r a) => a -> c -> JSIO (Either ScreepsError ())
transfer amt r a c = action4 "transfer" prim__transfer amt r a c

export
withdrawAll : (ToFFI c Creep) => (r : Resource) -> (MayHoldResource r a) => a -> c -> JSIO (Either ScreepsError ())
withdrawAll r a c = action3 "withdrawAll" prim__withdrawAll r a c

export
withdraw : (ToFFI c Creep) => Int32 -> (r : Resource) -> (MayHoldResource r a) => a -> c -> JSIO (Either ScreepsError ())
withdraw amt r a c = action4 "withdraw" prim__withdraw amt r a c

export
getCapacity : (HasIO io) => (r: Resource) -> (MayHoldResource r a) => a -> io Int32
getCapacity r a = primIO $ prim__getCapacity r a

export
getFreeCapacity : (HasIO io) => (r: Resource) -> (MayHoldResource r a) => a -> io Int32
getFreeCapacity r a = primIO $ prim__getFreeCapacity r a

export
getUsedCapacity : (HasIO io) => (r: Resource) -> (MayHoldResource r a) => a -> io Int32
getUsedCapacity r a = primIO $ prim__getUsedCapacity r a
