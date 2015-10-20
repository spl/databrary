'use strict'

app.directive 'volumeMeter', [
  '$compile', '$templateCache', 'constantService', 'displayService',
  ($compile, $templateCache, constants, display) ->
    restrict: 'E'
    templateUrl: 'volume/meter.html'
    require: '^wizard',
    scope:
      volumeFn: '&volume'
    link: ($scope, $element, $attrs, wizard) ->
      $scope.volume = volume = $scope.volumeFn()
      return unless volume
      $scope.name = 'meter'
      $scope.id = wizard.name + '-meter'
      $scope.complete = true
      $scope.tab = $compile($templateCache.get('volume/meterTab.html'))($scope)
      $scope.unstyled = true
      wizard.addStep($scope)

      class Metric
        constructor: (@test) ->
          return

        run: () ->
          @done = @test()

      $scope.metrics = metrics =
        name: new Metric ->
          !!volume.name
        description: new Metric ->
          volume.body?.length >= 500
        citation: new Metric ->
          !!volume.citation
        keywords: new Metric ->
          n = 0
          for t in volume.tags when t.keyword
            n++
            return true if n >= 3
        excerpts: new Metric ->
          n = 0
          for e in volume.excerpts when e.release >= constants.release.SHARED
            n++
            return true if n >= 3
        thumbnail: new Metric ->
          for e in volume.excerpts when e.release >= constants.release.PUBLIC and (e.format.type == 'image' || e.format.type == 'video')
            return true
        design: new Metric ->
          for m of volume.metrics
            return true
        session: new Metric ->
          for ci, c of volume.containers when !c.top
            return true
        record: new Metric ->
          for ri, r of volume.records
            return true
        asset: new Metric ->
          for ai, a of volume.assets when a.container
            return true
        share: new Metric ->
          volume.accessPreset

      $scope.goto = (step, target) ->
        wizard.activateStep(step)
        display.scrollTo(target) if target
        return

      calculate = () ->
        d = 0
        t = 0
        for n, m of metrics
          d++ if m.run()
          t++
        $scope.meter = d / t
        return

      calculate()
]
