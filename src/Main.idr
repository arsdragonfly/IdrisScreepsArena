module Main

import IdrisScreepsArena
import Data.List
import JS

%default total

-- loop and import
tutorial1: (HasIO io) => io ()
tutorial1 = do
  ticks <- getTicks
  putStrLn $ show ticks

simpleMove2: IO ()
simpleMove2 = do
  creeps <- getObjectsByPrototypeCreep
  flags <- getObjectsByPrototypeFlag
  let firstCreep : (Maybe Creep) = head' creeps
  let firstFlag : (Maybe Flag) = head' flags
  let result : (Maybe $ IO ()) = (the (Maybe $ IO ()) (?hole <$> firstCreep <*> firstFlag))
  pure ()
-- TODO: find out why this won't work
-- idiom brackets, <$> and <*>, and bang patterns here are all broken
{-
  case (the (Maybe $ IO ()) [| moveTo firstCreep firstFlag |]) of
       Just _ => pure ()
       Nothing => pure ()
-}

-- simple move
tutorial2 : JSIO ()
tutorial2 = do
  Just creep <- map head' getObjectsByPrototypeCreep
    | Nothing => consoleLog "creep not found"
  Just flag <- map head' getObjectsByPrototypeFlag
    | Nothing => consoleLog "target not found"
  Right _ <- moveTo creep flag
    | Left err => consoleLog $ jsShow err
  consoleLog "approaching..."

-- first attack
tutorial3 : JSIO ()
tutorial3 = do
  creeps <- getObjectsByPrototypeCreep
  Just creep <- map head' $ filterM my creeps
    | Nothing => consoleLog "creep not found"
  Just enemy <- map head' $ filterM ((map not) . my) creeps
    | Nothing => consoleLog "enemy not found"
  Right okay <- attack creep enemy
    | Left _ => do
      Right _ <- moveTo creep enemy
        | Left err => consoleLog $ jsShow err
      consoleLog "approaching..."
  consoleLog "attacking..."

data CreepAction = RangedAttackAction | AttackAction

getBodypart : CreepAction -> Bodypart
getBodypart RangedAttackAction = RangedAttack
getBodypart AttackAction = Attack

GetConstraint : CreepAction -> (Type -> Type)
GetConstraint RangedAttackAction = Attackable
GetConstraint AttackAction = Attackable

0 ConcreteActionType : CreepAction -> Type
ConcreteActionType ca = (forall o . (GetConstraint ca) o => Creep -> o -> JSIO (Either ScreepsError ()))

-- TODO: generalize this to multiple actions per bodypart
concreteAction : (ca : CreepAction) -> ConcreteActionType ca
concreteAction RangedAttackAction = rangedAttack
concreteAction AttackAction = attack

concreteActionWhenApplicable : (ca : CreepAction) -> ConcreteActionType ca
concreteActionWhenApplicable ca = (\creep, target => do
  bodyparts <- body creep
  case any (\x => x.type == getBodypart ca) bodyparts of
    True => concreteAction ca creep target
    False => pure (Right ()))

-- if either rangedAttack or attack fails due to out of range, move to target
moveToAssault : (Attackable o) => o -> Creep -> JSIO ()
moveToAssault target creep = do
  bodyparts <- body creep
  rangedAttackResult <- concreteActionWhenApplicable RangedAttackAction creep target
  attackResult <- concreteActionWhenApplicable AttackAction creep target
  let r1 = rangedAttackResult
  let r2 = attackResult
  case (the (Either _ _) (rangedAttackResult >> attackResult)) of
    Right _ => pure ()
    Left _ => ignore (moveTo creep target)

-- creeps bodies
-- TODO: healing
tutorial4 : JSIO ()
tutorial4 = do
  creeps <- getObjectsByPrototypeCreep
  Just enemy <- map head' $ filterM ((map not) . my) creeps
    | Nothing => consoleLog "enemy has been eliminated."
  myCreeps <- filterM my creeps
  ignore $ traverse (moveToAssault enemy) myCreeps
  consoleLog "attacking..."

main : IO ()
main = runJS tutorial4
