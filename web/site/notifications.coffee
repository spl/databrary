'use strict'

app.directive 'notifications', [
  '$sce', 'routerService',
  ($sce, router) ->
    restrict: 'E'
    templateUrl: 'site/notifications.html'
    scope: {}
    link: ($scope) ->
      router.http(router.controllers.getNotifications).then (res) ->
          for n in $scope.notifications = res.data
            n.html = $sce.trustAsHtml(n.html)
          return
        , (res) ->
          messages.addError
            type: 'red'
            body: 'An error occured retrieving your notifications; please try again'
            report: res
          return
      return
]
