module Main

import Data.List
import JS

data Creep : Type where [external]

data Flag : Type where [external]

%foreign "javascript:lambda:(u, x) => x.x"
prim__x : forall a . a -> Int

%foreign "javascript:lambda:(u, x) => x.y"
prim__y : forall a . a -> Int

interface HasPosition a where
  posX : a -> Int
  posX = prim__x
  posY : a -> Int
  posY = prim__y

HasPosition Creep where

HasPosition Flag where

-- TODO: handle return values
%foreign "javascript:lambda:(u, creep, target) => creep.moveTo(target)"
prim__moveTo : forall a . Creep -> a -> PrimIO ()

moveTo : (HasIO io, HasPosition o) => Creep -> o -> io ()
moveTo creep o = primIO (prim__moveTo creep o)

moveTo2 : (HasPosition o) => Creep -> o -> IO ()
moveTo2 = moveTo

%foreign "javascript:lambda: () => getObjectsByPrototype(Creep)"
prim__getObjectsByPrototypeCreep : () -> PrimIO (IArray Creep)

getObjectsByPrototypeCreep : (HasIO io) => () => io (IArray Creep)
getObjectsByPrototypeCreep = primIO (prim__getObjectsByPrototypeCreep ())

%foreign "javascript:lambda: () => getObjectsByPrototype(Flag)"
prim__getObjectsByPrototypeFlag : () -> PrimIO (IArray Flag)

getObjectsByPrototypeFlag : (HasIO io) => () => io (IArray Flag)
getObjectsByPrototypeFlag = primIO (prim__getObjectsByPrototypeFlag ())

%foreign "javascript:lambda: () => getTicks()"
prim__getTicks : () -> Int

printTicks: IO ()
printTicks = do
  putStrLn $ show (prim__getTicks ())

simpleMove: IO ()
simpleMove = do
  creeps <- getObjectsByPrototypeCreep
  flags <- getObjectsByPrototypeFlag
  let firstCreep = readMaybe creeps 0
  let firstFlag = readMaybe flags 0
  case firstCreep of
       Nothing => pure ()
       Just c => case firstFlag of
                      Nothing => pure ()
                      Just f => do
                        consoleLog "moving..."
                        moveTo c f

simpleMove2: IO ()
simpleMove2 = do
  creeps <- getObjectsByPrototypeCreep
  flags <- getObjectsByPrototypeFlag
  let firstCreep : (Maybe Creep) = readMaybe creeps 0
  let firstFlag : (Maybe Flag) = readMaybe flags 0
  let result : (Maybe $ IO ()) = (the (Maybe $ IO ()) (?hole <$> firstCreep <*> firstFlag))
  pure ()
{-
  case (the (Maybe $ IO ()) [| moveTo firstCreep firstFlag |]) of
       Just _ => pure ()
       Nothing => pure ()
-}

simpleMove3: IO ()
simpleMove3 = do
  creeps <- getObjectsByPrototypeCreep
  flags <- getObjectsByPrototypeFlag
  let firstCreep = readMaybe creeps 0
  let firstFlag = readMaybe flags 0
  let result = do
    creep <- firstCreep
    flag <- firstFlag
    pure (the (IO ()) (moveTo creep flag))
  case result of
       Nothing => pure ()
       Just io => io

main : IO ()
main = simpleMove

