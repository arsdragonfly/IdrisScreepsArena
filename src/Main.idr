module Main

import IdrisScreepsArena
import Data.List
import JS

-- TODO: handle return values
%foreign "javascript:lambda:(u, creep, target) => creep.moveTo(target)"
prim__moveTo : forall a . Creep -> a -> PrimIO ()

%foreign "javascript:lambda:(u, creep, target) => creep.moveTo(target)"
prim__moveToWithReturn : forall a . Creep -> a -> PrimIO (Union2 ScreepsError ScreepsOK)

moveTo : (HasIO io, HasPosition o) => Creep -> o -> io ()
moveTo creep o = primIO (prim__moveTo creep o)

moveTo2 : (HasPosition o) => Creep -> o -> IO ()
moveTo2 = moveTo

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

simpleMove: IO ()
simpleMove = do
  creeps <- getObjectsByPrototypeCreep
  flags <- getObjectsByPrototypeFlag
  let action = do
    creep <- readMaybe creeps 0
    flag <- readMaybe flags 0
    pure (the (IO ()) (moveTo creep flag))
  case action of
       Nothing => consoleLog "creep or target not found"
       Just action => action

main : IO ()
main = simpleMove

