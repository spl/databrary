'use strict'

app.directive 'accessSearchForm', [
  'constantService',
  (constants) ->
    restrict: 'E'
    templateUrl: 'volume/accessSearch.html',
    link: ($scope, $element, $attrs) ->
      volume = $scope.volume
      form = $scope.accessSearchForm

      form.placeholderText = $attrs.placeholderText || constants.message('access.search.placeholder')

      select = (found) -> ->
        $scope.selectFn(found)
        form.$setPristine()
        ''

      form.search = (val) ->
        volume.accessSearch(val).then (data) ->
              form.validator.server {}
              for found in data
                text: found.name
                select: select(found)
            , (res) ->
              form.validator.server res
              return

      form.validator.client
          name:
            tips: constants.message('access.search.name.help')
        , true

      return
]
