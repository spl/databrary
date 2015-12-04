'use strict'

app.directive 'activity', [
  'constantService',
  (constants) ->
    restrict: 'E'
    templateUrl: 'site/activity.html'
    link: ($scope) ->
      $scope.activity.reverse()

      $scope.idColor = (i) ->
        hsv = 3733*i
        sv = hsv/360|0
        "hsl(#{hsv%360},#{70+sv%20}%,#{45+(sv/20|0)%27}%)"

      isAuthRequest = (act) ->
        act.type == 'authorize' && act.action == 'add' && act.party.id == act.user && !act.site && !act.member && !act.old

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
        preset:
          [
            {
              add: "shared selected information with the public"
              remove: "stopped sharing with the public"
            }, {
              add: "shared with authorized researchers"
              remove: "stopped sharing with authorized researchers"
            }
          ]
        release:
          add: "set"
          remove: "cleared"
        asset:
          add: "uploaded"

      $scope.activityAction = (act) ->
        if isAuthRequest(act)
          return "requested"
        at = action[act.type]
        if act.type == 'access' && (i = constants.accessPreset.parties.indexOf(act.party.id)) >= 0
          at = action.preset[i]
        a = at?[act.action] ? action[act.action]
        if act.type == 'container'
          a += ' ' + (if act.top then 'materials' else 'session')
        a
]
