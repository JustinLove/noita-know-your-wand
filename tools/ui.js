var imageDataURI = require('image-data-uri')
var fs = require('fs')
var path = require('path')

var dir = '../public/data/ui_gfx/inventory'
fs.readdir(dir, function(err, files) {
  if (err) return console.log(err);
  Promise.all(files.map(function(file){
      return imageDataURI.encodeFromFile(path.join(dir, file)).then(function(uri) {return {file: file, uri: uri}})
    }))
    .then(function(results) {
      var text = 'module Sprite.UiSprite exposing(..)\n' + results.map(function(res) {
        return res.file.replace('.png', '') + ' = "' + res.uri + '"'
      }).join('\n')
      return fs.writeFile('../generated/Sprite/UiSprite.elm', text, function(err) {
        if (err) console.log(err)
      })
    })
    .catch(function(err) {
      console.log(err)
    })
})


