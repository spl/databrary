'use strict'

app.directive 'volumeDesign', [
  '$location', 'constantService', 'routerService', 'messageService',
  ($location, constants, router, messages) ->
    restrict: 'E'
    templateUrl: 'volume/design.html'
    link: ($scope) ->
      volume = $scope.volume
      form = $scope.volumeDesign

      $scope.select = (c) ->
        form.metric = {}
        return unless ($scope.selected = constants.category[c])?
        $location.replace().search('key', volume.metrics[c] && c)
        for m in volume.metrics[c] ? $scope.selected.template
          form.metric[m] = true
        return

      init = () ->
        form.category = {}
        for c of volume.metrics
          form.category[c] = true
        $scope.select($scope.selected?.id || $location.search().key)
        return
      init()

      $scope.change = (c, m) ->
        return unless c?
        messages.clear(form)
        form.$setSubmitted()
        volume.setVolumeMetrics(c, m, if m? then form.metric[m] else form.category[c]).then(() ->
            init()
            form.$setPristine()
            return
          , (res) ->
            init()
            form.$setUnsubmitted()
            messages.addError
              body: 'Error changing volume design'
              report: res
              owner: form
            return)
        return

      $scope.stop = (e) ->
        e.stopPropagation()
        return false
]
