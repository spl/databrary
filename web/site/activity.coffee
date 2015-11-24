'use strict'

app.directive 'activity', [
  () ->
    restrict: 'E'
    templateUrl: 'site/activity.html'
    link: ($scope) ->
      $scope.activity.reverse()

      $scope.idColor = (i) ->
        hsv = 3733*i
        sv = hsv/360|0
        "hsl(#{hsv%360},#{70+sv%20}%,#{45+(sv/20|0)%27}%)"

      action =
        add: "created"
        remove: "removed"
        change: "changed"
        superuser: "superuser"
        party:
          add: "registered"
        account:
          add: "created account"
          attempt: "failed login"
          open: "logged in"
          close: "logged out"
        authorize:
          add: "added"
        volume:
          add: "created volume"
          change: "changed volume"
        access:
          add: "added"
        release:
          add: "set"
          remove: "cleared"
        asset:
          add: "uploaded"
      $scope.activityAction = (act) ->
        a = action[act.type]?[act.action] ? action[act.action]
        if act.type == 'container'
          a += ' ' + (if act.top then 'materials' else 'session')
        a
]
