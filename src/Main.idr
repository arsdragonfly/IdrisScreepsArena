module Main

import IdrisScreepsArena
import Data.List
import Data.SOP
import Generics.SOP
import Generics.Derive
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
  let firstCreep : (Maybe Creep) = readMaybe creeps 0
  let firstFlag : (Maybe Flag) = readMaybe flags 0
  let result : (Maybe $ IO ()) = (the (Maybe $ IO ()) (?hole <$> firstCreep <*> firstFlag))
  pure ()
-- TODO: find out why this won't work
{-
  case (the (Maybe $ IO ()) [| moveTo firstCreep firstFlag |]) of
       Just _ => pure ()
       Nothing => pure ()
-}

simpleMove: JSIO ()
simpleMove = do
  creeps <- getObjectsByPrototypeCreep
  flags <- getObjectsByPrototypeFlag
  let action = do
    creep <- readMaybe creeps 0
    flag <- readMaybe flags 0
    pure (the (JSIO _) (moveTo creep flag))
  case action of
       Nothing => consoleLog "creep or target not found"
       Just action => do
         moveResult <- action
         pure ()

main : IO ()
main = runJS simpleMove

