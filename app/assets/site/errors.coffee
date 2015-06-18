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
        display.title = constants.message('error.title')
        error.message ?= constants.message(
            if (msg = 'error.' + error.status + '.message') of constants.messages
              msg
            else
              'error.resolve'
          , {sce:$sce.HTML})
        $scope.error = error
        return
      return
])
