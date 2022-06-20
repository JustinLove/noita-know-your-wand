#!/usr/bin/env bash
elm make src/NoitaKnowYourWand.elm --optimize --output public/noita-know-your-wand.max.js
cd tools
npx esbuild ../public/noita-know-your-wand.max.js --minify --target=es5 --outfile=../public/noita-know-your-wand.js
rm ../public/noita-know-your-wand.max.js
