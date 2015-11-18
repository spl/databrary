'use strict'

app.controller 'volume/slotActivity', [
  '$scope', 'displayService', 'slot',
  ($scope, display, slot) ->
    display.title = slot.displayName + " activity"
    $scope.slot = slot
    $scope.activity = slot.activity
    return
]
