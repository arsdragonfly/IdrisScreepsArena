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

(TFFI a b, TJSON b) => TJSON a where
  tJSON = tJSON . tFFI

test : ()
test = tJSON AA

-- interface TFFIExists a where
--   constructor MkTFFIExists
--   TFFITarget : Type
--   tFFIImplementation : TFFI a TFFITarget

-- %hint
-- tFFIExistsA : {auto prf: TFFI A B} -> TFFIExists A
-- tFFIExistsA = MkTFFIExists B prf

-- interface (TFFIExists a) => TFFISatisfiesConstraint (0 c: Type -> Type) a where
--   constructor MkTFFISatisfiesConstraint
--   toFFITargetConstraintEvidence : c (TFFITarget {a})

-- %hint
-- withTFFIConstraint : (impl: TFFIExists a) -> c (TFFITarget@{impl}) -> TFFISatisfiesConstraint c a
-- withTFFIConstraint impl cb = MkTFFISatisfiesConstraint cb

-- interface (TFFISatisfiesConstraint TJSON a) => Success a where
--   constructor MkSuccess
--   success : a -> ()
--   success _ = ()

-- TFFISatisfiesConstraint TJSON a => Success a where

-- test : ()
-- test = tJSON AA

-- suc : {auto p : TFFISatisfiesConstraint TJSON A} -> A -> ()
-- suc = success

-- suc : (p : TFFISatisfiesConstraint TJSON A) => A -> ()
-- suc = success

-- typ : A -> B
-- typ a = tFFI@{tFFIImplementation} a

-- suc : A -> ()
-- suc = success

-- go : ()
-- go = suc AA
