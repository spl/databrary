'use strict'

app.controller 'asset/volumeZipView', [
  '$scope', 'displayService', 'asset',
  ($scope, display, asset) ->
    $scope.close = ->
      window.history.back()
    console.log asset
    display.title = asset.displayName
    $scope.volume = asset.volume
 ]
