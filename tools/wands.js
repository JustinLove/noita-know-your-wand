var imageDataURI = require('image-data-uri')
var fs = require('fs')
var path = require('path')

var dir = '../public/data/items_gfx/wands'
fs.readdir(dir, function(err, files) {
  if (err) return console.log(err);
  Promise.all(files.map(function(file){
      return imageDataURI.encodeFromFile(path.join(dir, file)).then(function(uri) {return {file: file, uri: uri}})
    }))
    .then(function(results) {
      var text = 'module Sprite.WandSprites exposing(wandSprites)\nimport Dict\nwandSprites = Dict.fromList [' + results.map(function(res) {
        return '("' + res.file + '","' + res.uri + '")'
      }).join(',\n  ') + ']'
      return fs.writeFile('../generated/Sprite/WandSprites.elm', text, function(err) {
        if (err) console.log(err)
      })
    })
    .catch(function(err) {
      console.log(err)
    })
})


