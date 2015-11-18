'use strict'

app.controller 'party/activity', [
  '$scope', 'displayService', 'party',
  ($scope, display, party) ->
    display.title = party.name + " activity"
    $scope.party = party
    $scope.activity = party.activity
    return
]
