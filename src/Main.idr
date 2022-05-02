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
  creeps <- getObjects Creep
  flags <- getObjects Flag
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
  Just creep <- map head' $ getObjects Creep
    | Nothing => consoleLog "creep not found"
  Just flag <- map head' $ getObjects Flag
    | Nothing => consoleLog "target not found"
  Right _ <- moveTo flag creep
    | Left err => consoleLog $ jsShow err
  consoleLog "approaching..."

-- first attack
tutorial3 : JSIO ()
tutorial3 = do
  creeps <- getObjects Creep
  Just creep <- map head' $ filterM my creeps
    | Nothing => consoleLog "creep not found"
  Just enemy <- map head' $ filterM ((map not) . my) creeps
    | Nothing => consoleLog "enemy not found"
  Right okay <- attack enemy creep
    | Left _ => do
      Right _ <- moveTo enemy creep
        | Left err => consoleLog $ jsShow err
      consoleLog "approaching..."
  consoleLog "attacking..."

data CreepAction = RangedAttackAction | AttackAction | HealAction

getBodypart : CreepAction -> Bodypart
getBodypart RangedAttackAction = RangedAttack
getBodypart AttackAction = Attack
getBodypart HealAction = Heal

GetConstraint : CreepAction -> (Type -> Type)
GetConstraint RangedAttackAction = Attackable
GetConstraint AttackAction = Attackable
GetConstraint HealAction = (\t => ToFFI t Creep)

0 ConcreteActionType : CreepAction -> Type
ConcreteActionType ca = (forall o . (GetConstraint ca) o => o -> Creep -> JSIO (Either ScreepsError ()))

-- TODO: generalize this to multiple actions per bodypart
concreteAction : (ca : CreepAction) -> ConcreteActionType ca
concreteAction RangedAttackAction = rangedAttack
concreteAction AttackAction = attack
concreteAction HealAction = heal

concreteActionWhenApplicable : (ca : CreepAction) -> ConcreteActionType ca
concreteActionWhenApplicable ca = (\target, creep => do
  bodyparts <- body creep
  case any (\x => x.type == getBodypart ca) bodyparts of
    True => concreteAction ca target creep
    False => pure (Right ()))

-- if either rangedAttack or attack fails due to out of range, move to target
moveToAssault : (Attackable o) => o -> Creep -> JSIO ()
moveToAssault target creep = do
  bodyparts <- body creep
  rangedAttackResult <- concreteActionWhenApplicable RangedAttackAction target creep
  attackResult <- concreteActionWhenApplicable AttackAction target creep
  let r1 = rangedAttackResult
  let r2 = attackResult
  case (the (Either _ _) (rangedAttackResult >> attackResult)) of
    Right _ => pure ()
    Left _ => ignore (moveTo target creep)

moveToHeal : (ToFFI o Creep) => o -> Creep -> JSIO ()
moveToHeal target creep = do
  bodyparts <- body creep
  healResult <- concreteActionWhenApplicable HealAction target creep
  case (the (Either _ _) (healResult)) of
    Right _ => pure ()
    Left _ => ignore (moveTo target creep)

-- creeps bodies
tutorial4 : JSIO ()
tutorial4 = do
  creeps <- getObjects Creep
  Just enemy <- map head' $ filterM ((map not) . my) creeps
    | Nothing => consoleLog "enemy has been eliminated."
  myCreeps <- filterM my creeps
  ignore $ traverse (moveToAssault enemy) myCreeps
  consoleLog "attacking..."
  Just myWounded <- map head' $ filterM wounded myCreeps
    | Nothing => consoleLog "no need to heal."
  ignore $ traverse (moveToHeal myWounded) myCreeps
  consoleLog "healing..."

-- store and transfer
tutorial5 : JSIO ()
tutorial5 = do
  creeps <- getObjects Creep
  Just enemy <- map head' $ filterM ((map not) . my) creeps
    | Nothing => consoleLog "enemy has been eliminated."
  Just myCreep <- map head' $ filterM my creeps
    | Nothing => consoleLog "creep not found."
  Just myContainer <- map head' $ getObjects Container
    | Nothing => consoleLog "no containers found."
  Just myTower <- map head' $ (getObjects Tower) >>= filterM my
    | Nothing => consoleLog "no towers found."
  ignore $ attack enemy myTower
  amt <- getUsedCapacity Energy myCreep
  if amt > 0 then
    ignore $ transferAll Energy myTower myCreep else
    ignore $ withdrawAll Energy myContainer myCreep
  
  
main : IO ()
main = runJS tutorial5
