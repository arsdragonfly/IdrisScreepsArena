#!/bin/bash
echo 'Adding imports and exports...'
cp -f ./build/exec/IdrisScreepsArena ./main.mjs
cat header.mjs main.mjs > temp.mjs
perl -pi -e 's/const __mainExpression_0 = __lazy/const __mainExpression_0 = /g' temp.mjs
perl -pi -e 's/try{__mainExpression_0\(\)}(.*)/export function loop(){\ntry{return __mainExpression_0();}\1\n}/g' temp.mjs
mv temp.mjs main.mjs
echo 'Done.'
