# Noita, Know Your Wand

Web application where you can drag wand atributes to a box to redefine the table. Group wands into different arrangements to look for visual patterns.

Attribute values are approximate. Noita makes the wand and then tries to find the best match for the art, so some attributes may be out of range if the others match well.

I haven't been able to find a pattern to Spread or Reload Time.

## Compiling

Built using [Elm](http://elm-lang.org/)

My build command:

> `elm make src/NoitaKnowYourWand.elm --output public/noita-know-your-wand.js`

`bin/monitor.bat` has a command using the [watch](https://www.npmjs.com/package/watch) CLI

Once built, `public/index.html` can be opened locally from the filesystem, or set up for a local or internet server as you wish.

There is also ImportData.elm and `import_monitor.sh`. These process wand stats and graphics into Elm source code for the application. the generated files are part of the repo; you will need (perhaps a subset of) noita game files to run these from scratch.

```
public/data/items_graphics/wands/*.png
public/data/scripts/gun/procedural/wands.lua
public/data/ui_gfx/inventory/ select pngs, plus the semi-custom eye graphics were put here for easy of use
```

## Credits

Icons: [IcoMoon - Free](https://icomoon.io/#icons-icomoon) ([CC BY 4.0](http://creativecommons.org/licenses/by/4.0/))
