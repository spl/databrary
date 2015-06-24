'use strict'

app.controller 'site/search', [
  '$scope', '$location', 'displayService', 'results', 'pageService'
  ($scope, $location, display, results, page) ->
    console.log("results")
    console.log(results)

    $scope.selectedType = ""
    $scope.selectedVolume = ""
    $scope.selectedFilter = ""
    $scope.display = []
    $scope.selectSessionStr = "Volumes w/ Sessions"
    $scope.selectHighlightStr = "Volumes w/ Highlights"
    $scope.limit = 10

    # Put an ng-key enter here so it does what we offset
    $scope.search = ->
      # return page.router.http(page.router.controllers.postSearch, page.$route.current.params)
      console.log("prev results", results)
      console.log("query", $scope.query)
      console.log("page params", page.$route.current.params)
      # page.$route.current.params.query = $scope.query
      promise = page.router.http(page.router.controllers.postSearch,
        {"query" : $scope.query, "offset" : $scope.offset, "limit" : $scope.limit})
      console.log(promise)
      promise.then (res) ->
        console.log("GOT RES:", res)
        $scope.parseResults(res.data)

    #################################
    # Parse results, the main workhorse function
    # ##############################
    $scope.parseResults = (res) ->
      params = $location.search()
      $scope.query = params.query
      $scope.originalQuery = params.query

      # console.log("VOLUME RESULTS BEFORE", $scope.volumeResults)
      $scope.partyResults = $scope.getResults("party", res)
      $scope.volumeResults = $scope.getResults("volume", res)
      # console.log("VOLUME RESULTS AFTER", $scope.volumeResults)

      $scope.partyCount = $scope.getTypeCounts("party", res)
      $scope.volumeCount = $scope.getTypeCounts("volume", res)

      $scope.totalCount = $scope.partyCount + $scope.volumeCount

      $scope.minPage = 1
      $scope.maxPage = 1 + ($scope.totalCount / $scope.limit)
      pageRange = []
      for i in [$scope.minPage .. $scope.maxPage] by 1
        pageRange.push(i)

      $scope.pageRange = pageRange

      $scope.goToPage = (page) ->
        $scope.offset = $scope.limit * (page-1)
        $scope.search()

      if parseInt($scope.totalCount) > ($scope.offset + $scope.limit)
        $scope.next = ->
          $scope.offset = $scope.offset + $scope.limit
          $scope.search()
      if $scope.offset > 0
        $scope.prev = ->
          $scope.offset = Math.max(0, $scope.offset - $scope.limit)
          $scope.search()

      $scope.number = 1 + ($scope.offset / $scope.limit)


    $scope.findFacet = (typeName) ->
      facets = (facet.val for facet in results.facets.content_type.buckets)
      facets.indexOf(typeName)

    $scope.getTypeCounts = (type, res) ->
      idx = $scope.findFacet(type)
      if idx < 0
        return null
      return res?.facets?.content_type?.buckets[idx].count ? 0

    $scope.findResult = (typeName, res) ->
      groups = (group.groupValue for group in res.grouped.content_type.groups)
      groups.indexOf(typeName)

    $scope.getResults = (type, res) ->
      idx = $scope.findResult(type, res)
      if idx < 0
        return null
      return res.grouped.content_type.groups[idx].doclist

    $scope.getVolumeFeatureBoxOpts = ->
      opts = [$scope.selectSessionStr, $scope.selectHighlightStr]
      return opts

    $scope.partyVolBoxClick = ->
      if "Volumes" in $scope.selectedType
        $scope.display = (s for s in $scope.getVolumeFeatureBoxOpts())
      if "People" in $scope.selectedType
        $scope.display = (doc.party_name_s for doc in $scope.partyResults.docs)
      console.log($scope.selectedType, $scope.display)

    $scope.filterBoxClick = ->
      console.log("FILTER BOX CLICKED", $scope.selectedFilter)
      if $scope.selectSessionStr in $scope.selectedFilter
        console.log("FILTER BY SESSION")
        $scope.filterBySession()
      if $scope.selectHighlightStr in $scope.selectedFilter
        $scope.filterByHighlight()
      $scope.search()
      console.log(results)
      # $scope.$timeout(angular.noop)

      # funcs = {"session" : filterBySession, "highlight" : $scope.filterByHighlight}

    # Now we want to rerun the search but only return vols w/ highlights
    $scope.filterByHighlight = ->
      $scope.requireHighlight = true
      $scope.query = $scope.originalQuery + "|arg=volume_has_excerpt_b:true"

    $scope.filterBySession = ->
      $scope.requireSession = true
      $scope.query = $scope.originalQuery + "|arg=volume_has_sessions_b:true"


    console.log(results)
    $scope.parseResults(results)

    $scope.offset = parseInt($location.search().offset, 10) || 0
    display.title = 'Search'
    $scope.results = results?.response

    console.log(results)


    return
]
