'use strict'

app.controller 'volume/zip', [
  '$scope', 'constantService', 'displayService', 'volume', 'slot',
  ($scope, constants, display, volume, slot) ->

    if slot
      display.title = slot.displayName + ".zip"
      $scope.slot = slot
      $scope.volume = slot.volume
      $scope.containers = {}
      $scope.containers[slot.id] = slot
    else
      display.title = volume.name + ".zip"
      $scope.volume = volume
      $scope.containers = volume.containers
      $scope.contCount = Object.keys(volume.containers).length

    t = 0
    d = 0
    z = 0
    $scope.filesInSession = {}
    for ci, c of $scope.containers
      $scope.filesInSession[ci] = Object.keys(c.assets).length
      console.log c
      for ai, a of c.assets
        t++
        if a.checkPermission(constants.permission.VIEW)
          d++
          z += a.size
    $scope.assetTotal = t
    $scope.assetDownload = d
    $scope.assetSize = Math.ceil(z/(1024*1024))
    
    $scope.close = ->
      window.history.back()
      return
    return
 ]
