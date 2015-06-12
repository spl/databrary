'use strict'

app.controller 'volume/slotZip', [
  '$scope', 'displayService', 'slot', 'fileList'
  ($scope, display, slot, fileList) ->
    display.title = slot.displayName + ".zip"
    $scope.assets = fileList
    $scope.slot = slot
    $scope.volume = slot.volume
    $scope.close = ->
      window.history.back()
 ]

