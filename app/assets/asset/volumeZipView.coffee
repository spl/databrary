'use strict'

app.controller 'asset/volumeZipView', [
  '$scope', 'displayService','routerService', 'asset',
  ($scope, display, router, asset) ->
    $scope.close = ->
      window.history.back()
    console.log "Fart", router.controllers.VolumeApi.zipList
    router.http(router.controllers.VolumeApi.zipList, 5).then (list) ->
      $scope.assets = list.data
    console.log asset
    display.title = asset.displayName
    $scope.volume = asset.volume
 ]
