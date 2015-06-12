'use strict'

app.controller 'asset/slotZipView', [
  '$scope', 'displayService', 'slot', 'asset', 'routerService'
  ($scope, display, slot, asset,  router) ->
    $scope.close = ->
      window.history.back()
    router.http((router.controllers.SlotApi.zip), slot.volume.id, slot.id, slot.segment.format()).then (list) ->
      console.log list
      $scope.assets = list.data
    $scope.asset = $scope.slot = slot
    display.title = asset.displayName
    $scope.volume = asset.volume
 ]
