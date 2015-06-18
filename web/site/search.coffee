'use strict'

app.controller 'site/search', [
  '$scope', '$location', 'displayService', 'results',
  ($scope, $location, display, results) ->
    limit = 10 # server-side default
    offset = parseInt($location.search().offset, 10) || 0
    display.title = 'Search'
    $scope.results = results?.response
    $scope.number = 1 + (offset / limit)

    params = $location.search()
    $scope.query = params.query
    $scope.offset = offset
    $scope.limit = 10
    console.log($scope, $location)
    $scope.search = ->
      $location.search('query', $scope.query, 'offset', offset)


    $scope.minPage = 1
    $scope.maxPage = 1 + ($scope.results.numFound / limit)
    pageRange = []
    for i in [$scope.minPage .. $scope.maxPage] by 1
      pageRange.push(i)

    $scope.pageRange = pageRange
    console.log(pageRange)

    $scope.goToPage = (page) -> $location.search('offset', limit * page)

    if parseInt($scope.results.numFound) > (offset + limit)
      $scope.next = -> $location.search('offset', offset + limit)
#      $scope.results.docs.pop()
    if offset > 0
      $scope.prev = -> $location.search('offset', Math.max(0, offset - limit))
    return
]

# TODO
#app.controller 'site/search', [
#  '$location', 'constantService', 'routerService',
#  ($location, constants, routes) ->
#    restrict: 'E'
#    templateUrl: 'site/search.html'
#    scope: {}
#    link: ($scope, $element, $attrs) ->
#      $scope.types =
#        volume: 'Data'
#        principal: 'Authorized investigators'
#        institution: 'Authorized institutions'
#        party: 'All users and groups'
#      params = $location.search()
#      $scope.type = $attrs.type
#      if $scope.type == 'party'
#        if params.institution == 'true'
#          $scope.type = 'institution'
#        else if `params.access == constants.permission.EDIT` || $.isEmptyObject(params)
#          $scope.type = 'principal'
#      $scope.query = params.query
#      $scope.search = ->
#        (switch $scope.type
#          when 'volume'
#            $location.url(routes.volumeSearch())
#          when 'institution'
#            $location.url(routes.partySearch()).search({institution:'true',access:constants.permission.ADMIN})
#          when 'principal'
#            $location.url(routes.partySearch()).search({institution:'false',access:constants.permission.EDIT})
#          when 'party'
#            $location.url(routes.partySearch())
#          ).search('query', $scope.query)
#        return
#      return
#]
