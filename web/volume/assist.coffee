'use strict'

app.directive 'volumeAssist', [
  '$location', 'constantService', 'routerService', 'messageService'
  ($location, constants, router, messages) ->
    restrict: 'E'
    templateUrl: 'volume/assist.html'
    require: '^wizard',
    link: ($scope, $element, $attrs, wizard) ->
      volume = $scope.volume
      form = $scope.volumeAssist

]