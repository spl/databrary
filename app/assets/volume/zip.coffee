'use strict'

app.controller 'volume/zip', [
  '$scope', 'displayService', 'volume', 'fileList',
  ($scope, display, volume, fileList) ->
    display.title = volume.name + ".zip"
    $scope.assets = fileList
    $scope.volume = volume
    $scope.close = ->
      window.history.back()
 ]
