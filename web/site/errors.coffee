'use strict'

app.directive('errors', [
  '$sce', 'displayService', 'constantService', 'pageService',
  ($sce, display, constants, page) ->
    restrict: 'E'
    scope: {}
    templateUrl: 'site/errors.html'
    link: ($scope) ->
      $scope.page = page
      $scope.$on 'displayService-error', (event, error) ->
        display.title = "Error"
        error.title ?= constants.message(
            if (msg = 'error.r' + error.status) of constants.messages
              msg
            else
              'error.resolve'
          , {sce:$sce.HTML})
        error.message = constants.messages['error.r' + error.status + '.message']
        $scope.error = error
        return
      return
])
