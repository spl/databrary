'use strict'

app.directive 'volumeAssist', [
  () ->
    restrict: 'E'
    templateUrl: 'volume/assist.html'
    link: ($scope) ->
      volume = $scope.volume
      form = $scope.volumeAssist
      return
]
