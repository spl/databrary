'use strict'

app.directive 'volumeDesign', [
  'constantService', 'routerService', 'messageService',
  (constants, router, messages) ->
    restrict: 'E'
    templateUrl: 'volume/design.html'
    link: ($scope) ->
      volume = $scope.volume
      form = $scope.volumeDesign

      init = () ->
        for c of constants.category
          if cm = volume.metrics[c]
            f = {on:true}
            for m in cm
              f[m] = true
            form[c] = f
          else
            delete form[c]
      init()

      form.change = (c, m) ->
        messages.clear(form)
        form.$setSubmitted()
        volume.setVolumeMetrics(c, m, if m? then form[c][m] else form[c].on).then(() ->
            init()
            form.$setPristine()
            return
          , (res) ->
            init()
            form.$setUnsubmitted()
            messages.addError
              body: 'Error changing volume design'
              report: res
              owner: form)
]
