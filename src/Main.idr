module Main

import IdrisScreepsArena
import Data.List
import JS

%default total

printTicks: (HasIO io) => io ()
printTicks = do
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

simpleMove : JSIO ()
simpleMove = do
  Just creep <- map head' getObjectsByPrototypeCreep
    | Nothing => consoleLog "creep not found"
  Just flag <- map head' getObjectsByPrototypeFlag
    | Nothing => consoleLog "target not found"
  Right _ <- moveTo creep flag
    | Left err => consoleLog $ jsShow err
  consoleLog "approaching..."

firstAttack : JSIO ()
firstAttack = do
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

main : IO ()
main = runJS firstAttack

