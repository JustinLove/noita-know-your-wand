#!/usr/bin/env bash
watch "elm make src/ImportData.elm --output tools/ImportData.js" src/ & watch "node tools/import-data.js" tools/
#& watch "node tools/import-data.js && elm make src/NoitaKnowYourWand.elm --output public/noita-know-your-wand.js" tools/
