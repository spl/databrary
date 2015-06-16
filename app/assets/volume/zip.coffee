'use strict'

app.controller 'volume/zip', [
  '$scope', 'displayService', 'volume', 'slot', 'fileList',
  ($scope, display, volume, slot, fileList) ->

    if slot
      display.title = slot.displayName + ".zip"
      $scope.slot = slot
      $scope.volume = slot.volume
    else
      display.title = volume.name + ".zip"
      $scope.volume = volume

    $scope.assets = fileList
    $scope.assetCount = Object.keys(fileList).length
    
    $scope.close = ->
      window.history.back()
 ]
