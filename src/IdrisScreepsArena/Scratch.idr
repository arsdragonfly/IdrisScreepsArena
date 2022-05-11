module IdrisScreepsArena.Scratch

-- scratch file for quick POCs

import Data.List

data A = AA
data B = BB

interface TFFI a b | a where
  tFFI : a -> b

interface TJSON a where
  constructor MkTJSON
  tJSON : a -> ()

TJSON B where
  tJSON BB = ()

TFFI A B where
  tFFI AA = BB

interface TFFIExists a where
  constructor MkTFFIExists
  TFFITarget : Type
  tFFIImplementation : TFFI a TFFITarget

%hint
tFFIExistsA : {auto prf: TFFI A B} -> TFFIExists A
tFFIExistsA = MkTFFIExists B prf

interface (TFFIExists a) => TFFISatisfiesConstraint (0 c: Type -> Type) a where
  constructor MkTFFISatisfiesConstraint
  tFFITargetConstraintEvidence : c (TFFITarget {a})

%hint
withTFFIConstraint : (impl: TFFIExists a) -> c (TFFITarget@{impl}) -> TFFISatisfiesConstraint c a
withTFFIConstraint impl cb = MkTFFISatisfiesConstraint cb

-- export
-- exists : {k : Type} -> (k -> Type) -> Type
-- exists t = (b : Type) -> ({a : _} -> t a -> b) -> b

-- export
-- packExists : {k : Type} -> {t : k -> Type} -> {a : k} -> t a -> exists t
-- packExists x = \b, f => f x

-- export
-- unpackExists : {k : Type} -> {t : k -> Type} -> exists t -> {b : Type} -> ({a : k} -> t a -> b) -> b
-- unpackExists exT {b = b'} f = exT b' f

-- 0 TFFISatisfiesConstraint : (Type -> Type) -> Type -> Type
-- TFFISatisfiesConstraint constraint a = (b : Type ** ((TFFI a b), constraint b))

-- %hint
-- withFFI : TFFI a b -> TJSON b -> TJSON a
-- withFFI _ _ = MkTJSON (tJSON . tFFI)


interface (TFFISatisfiesConstraint TJSON a) => Success a where
  constructor MkSuccess
  success : a -> ()
  success _ = ()

TFFISatisfiesConstraint TJSON a => Success a where

-- test : ()
-- test = tJSON AA

-- suc : {auto p : TFFISatisfiesConstraint TJSON A} -> A -> ()
-- suc = success

-- suc : (p : TFFISatisfiesConstraint TJSON A) => A -> ()
-- suc = success

typ : A -> B
typ a = tFFI@{tFFIImplementation} a

suc : A -> ()
suc = success

go : ()
go = suc AA

export
exists : (Type -> Type) -> Type
exists c = {b : Type} -> ({a : Type} -> {prf : c a} -> a -> b) -> b

export
packExists : {c : Type -> Type} -> {a : Type} -> {auto prf : c a} -> a -> exists c
packExists x = \f => f {a} {prf} x

export
unpackExists : {c : Type -> Type} -> exists c -> {b : Type} -> ({a : Type} -> {prf : c a} -> a -> b) -> b
unpackExists exT = exT

foo : exists (TFFISatisfiesConstraint TJSON)
-- foo = packExists {c=TFFISatisfiesConstraint TJSON} {a=A} {prf=prf} AA
foo = packExists AA

listFoo : List (exists (TFFISatisfiesConstraint TJSON))
listFoo = [foo]

eliminateTJSON : {auto a : Type} -> {auto prf : TFFISatisfiesConstraint TJSON a} -> a -> ()
eliminateTJSON x = tJSON@{tFFITargetConstraintEvidence@{prf}} (tFFI@{tFFIImplementation} x)

eliminate : exists (TFFISatisfiesConstraint TJSON) -> ()
eliminate x = x eliminateTJSON

listUnit : List ()
listUnit = map eliminate listFoo