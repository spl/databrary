'use strict'

app.controller 'volume/activity', [
  '$scope', 'displayService', 'volume',
  ($scope, display, volume) ->
    display.title = volume.name + " activity"
    $scope.volume = volume
    $scope.activity = volume.activity
]
