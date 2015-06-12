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
    $scope.volume = slot.volume
    console.log slot.volume
 ]

