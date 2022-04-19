#!/bin/bash
echo 'Building Idris project...'
idris2 --build IdrisScreepsArena.ipkg --cg javascript

echo 'Adding imports and exports...'
cp -f ./build/exec/IdrisScreepsArena ./main.mjs
cat header.mjs main.mjs > temp.mjs
sed -i -e '$s/try{\([^}]*\?\)}\(.*\)/export function loop(){\ntry{return PrimIO_unsafePerformIO(x => Main_main(x));}\2\n}/' temp.mjs
mv temp.mjs main.mjs

echo 'Done.'
