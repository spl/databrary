'use strict'

app.controller 'site/search', [
  '$scope', '$location', 'displayService', 'results',
  ($scope, $location, display, results) ->
    console.log("results")
    console.log(results)

    $scope.selectedType = ""
    $scope.selectedVolume = ""
    $scope.display = []

    $scope.findFacet = (typeName) ->
      facets = (facet.val for facet in results.facets.content_type.buckets)
      facets.indexOf(typeName)

    $scope.getTypeCounts = (type) ->
      idx = $scope.findFacet(type)
      if idx < 0
        return null
      return results.facets.content_type.buckets[idx].count

    $scope.findResult = (typeName) ->
      groups = (group.groupValue for group in results.grouped.content_type.groups)
      groups.indexOf(typeName)

    $scope.getResults = (type) ->
      idx = $scope.findResult(type)
      if idx < 0
        return null
      return results.grouped.content_type.groups[idx].doclist

    $scope.partyVolBoxClick = ->
      if "Volumes" in $scope.selectedType
        $scope.display = (doc.title_t for doc in $scope.volumeResults.docs)
      if "People" in $scope.selectedType
        $scope.display = (doc.party_name_s for doc in $scope.partyResults.docs)
      console.log($scope.selectedType, $scope.display)


    $scope.partyResults = $scope.getResults("party")
    $scope.volumeResults = $scope.getResults("volume")

    $scope.partyCount = $scope.getTypeCounts("party")
    $scope.volumeCount = $scope.getTypeCounts("volume")

    $scope.totalCount = $scope.partyCount + $scope.volumeCount

#    console.log(partyResults, partyCount)
    limit = 10 # server-side default
    offset = parseInt($location.search().offset, 10) || 0
    display.title = 'Search'
    $scope.results = results?.response
    $scope.number = 1 + (offset / limit)

    console.log(results)

    params = $location.search()
    $scope.query = params.query
    $scope.offset = offset
    $scope.limit = 10

    # Put an ng-key enter here so it does what we offset
    $scope.search = ->
      $location.search('query', $scope.query, 'offset', offset)

#      We want the query to look something like this:
#    facet=true&facet.field=content_type&facet.mincount=1&group=true&group.field=content_type&group.limit=10

    # group.offset is offset
    # group.limit is limit

#    $scope.containersForVoume = (volume_id) ->
#      $location.search('query', $scope.query, 'offset', offset)



    $scope.minPage = 1
    $scope.maxPage = 1 + ($scope.totalCount / limit)
    pageRange = []
    for i in [$scope.minPage .. $scope.maxPage] by 1
      pageRange.push(i)

    $scope.pageRange = pageRange

    $scope.goToPage = (page) -> $location.search('offset', limit * (page-1))

    if parseInt($scope.totalCount) > (offset + limit)
      $scope.next = -> $location.search('offset', offset + limit)
    if offset > 0
      $scope.prev = -> $location.search('offset', Math.max(0, offset - limit))
    return
]