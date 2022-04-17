module Main

%foreign "javascript:lambda: () => getTicks()"
prim__getTicks : PrimIO Int

main : IO ()
main = do
    ticks <- fromPrim prim__getTicks
    putStrLn $ "Current tick:" ++ show ticks
