'use strict'

app.controller 'asset/volumeZipView', [
  '$scope', 'displayService','routerService', 'asset','volume', 'fileList',
  ($scope, display, router, asset,volume, fileList) ->
    $scope.close = ->
      window.history.back()
    console.log "Volume:", volume
    $scope.assets = fileList.data
    console.log asset
    display.title = asset.displayName
    $scope.volume = asset.volume
 ]
