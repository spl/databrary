'use strict'

app.directive 'volumeAssist', ['uploadService'
  (uploads) ->
    restrict: 'E'
    templateUrl: 'volume/assist.html'
    link: ($scope) ->
      volume = $scope.volume
      form = $scope.volumeAssist
      $scope.flowOptions = uploads.flowOptions()
      $scope.wantAssist = true

      $scope.placeholderFiles = [1,2,3]
      return
]
