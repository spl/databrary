'use strict'

app.directive 'activityChange', [
  'constantService',
  (constants) ->
    restrict: 'A'
    transclude: 'element'
    priority: 500
    link: ($scope, $element, $attrs, ctrl, $transclude) ->
      [act, type, field] = $attrs.activityChange.split('.', 3)
      act = $scope[act]
      val =
        old: act.old?[field]
        new: act[type][field]

      return unless val.old? || val.new?

      dt = $('<dt/>').text($attrs.title || constants.message($attrs.titleMessage))
      dd = $('<dd/>')
      add = (which) ->
        return unless val[which]?
        scope = $scope.$new()
        scope.value = val[which]
        $transclude(scope, (clone) ->
          clone.addClass('activity-'+which)
          dd.append(clone))
      add('old')
      dd.append(if 'block' of $attrs then '&darr;' else ' &rarr; ')
      add('new')

      $element.after(dt, dd)
      return
]
