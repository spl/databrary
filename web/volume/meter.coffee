'use strict'

app.directive 'volumeMeter', [
  '$compile', '$templateCache', 'constantService',
  ($compile, $templateCache, constants) ->
    restrict: 'E'
    templateUrl: 'volume/meter.html'
    require: '^wizard',
    scope:
      volumeFn: '&volume'
    link: ($scope, $element, $attrs, wizard) ->
      $scope.volume = volume = $scope.volumeFn()
      $scope.name = 'meter'
      $scope.id = wizard.name + '-meter'
      $scope.complete = true
      $scope.tab = $compile($templateCache.get('volume/meterTab.html'))($scope)
      $scope.unstyled = true
      wizard.addStep($scope)

      $scope.goto = wizard.activateStep
      $scope.metrics = metrics =
        name:
          test: () -> !!volume.name
          title: 'Volume created and named'
          help: 'Create your volume and give it a title.'
          link: 'overview'
        description:
          test: () -> volume.body?.length >= 500
          title: 'Description written'
          help: 'Add a description of at least 500 characters that summarizes what your volume contains and how the data were collected to help others find it.'
          link: 'overview'
        citation:
          test: () -> !!volume.citation
          title: 'Publication linked'
          help: 'Link your volume to any corresponding published work by adding a DOI or citation.'
          link: 'overview'
        keywords:
          test: () ->
            n = 0
            for t in volume.tags when t.keyword
              n++
              return true if n >= 3
          title: 'Keywords added'
          help: 'Add at least three keywords to label your volume.'
          link: 'overview'
        excerpts:
          test: () ->
            n = 0
            for e in volume.excerpts when e.release >= constants.release.SHARED
              n++
              return true if n >= 3
          title: 'Highlights selected'
          help: 'Select at least three highlights from your data to represent your volume.'
          link: 'data'
        thumbnail:
          test: () ->
            for e in volume.excerpts when e.release >= constants.release.PUBLIC and (e.format.type == 'image' || e.format.type == 'video')
              return true
          title: 'Thumbnail chosen'
          help: 'Select a public highlight from a video or image to use as your volume thumbnail.'
          link: 'data'
        design:
          test: () ->
            for m of volume.metrics
              return true
          title: 'Specify study design'
          help: 'Choose the categories and fields by which you organize your data.'
          link: 'design'
        session:
          test: () ->
            for ci, c of volume.containers when !c.top
              return true
          title: 'Create a session'
          help: 'Create at least one session in order to start uploading data.'
          link: 'data'
        record:
          test: () ->
            for ri, r of volume.records
              return true
          title: 'Create a label'
          help: 'Enter a label to use for your data such as participant, task, condition, etc.'
          link: 'data'
        asset:
          test: () ->
            for ai, a of volume.assets when a.container
              return true
          title: 'Upload a file'
          help: 'Upload at least one file to your volume.'
          link: 'data'
        share:
          name: 'share'
          test: () -> volume.accessPreset
          title: 'Share your volume'
          help: 'Share your volume with other Databrary users.'
          link: 'access'

      calculate = () ->
        d = 0
        t = 0
        for n, m of metrics
          d++ if m.done = m.test()
          t++
        $scope.meter = d / t

      calculate()
]
