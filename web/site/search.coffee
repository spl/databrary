'use strict'

app.controller 'site/search', [
  '$scope', '$location', 'displayService', 'results', 'pageService'
  ($scope, $location, display, results, page) ->
    console.log("results")
    console.log(results)

  ###########################
  # Some constants
  ###########################
    $scope.selectedType = ""
    $scope.selectedVolume = ""
    $scope.selectedFilter = ""
    $scope.display = []
    $scope.selectSessionStr = "Volumes w/ Sessions"
    $scope.selectHighlightStr = "Volumes w/ Highlights"
    $scope.selectAffiliationStr = "Volumes w/ Sessions"
    $scope.partyLinkPrefix = "party/"
    $scope.volumeLinkPrefix = "volume/"
    $scope.affiliations = []
    # $scope.selectHighlightStr = "Volumes w/ Highlights"
    $scope.limit = 10
    display.title = 'Search'


  ###########################
  # Functions for transforming a document into strings
  ###########################
    $scope.formPartyLink = (doc) ->
      return partyLinkPrefix + doc.party_id_i

    $scope.formVolumeLink = (doc) ->
      return volumeLinkPrefix + doc.volume_id_i

    $scope.formVolumeResult = (doc) ->
      # Form everything except the title, which we'll do in HTML so we can make it the link
      res = []
      if doc.abs_t
         if doc.abs_t.length > 150
            doc.abs_t = doc.abs_t[0..150] + "..."
         res.push doc.abs_t
      if doc.citation_t
        res.push doc.citation_t
      # Add more stuff here if we want it
      return res.join("\n")

  ###########################
  # Search handlers
  ###########################
    $scope.searchBox = ->
      $scope.query = $scope.originalQuery
      $scope.offset = 0
      console.log("NEW SEARCH:", $scope.query)
      $scope.search()

    # Put an ng-key enter here so it does what we offset
    $scope.search = ->
      # return page.router.http(page.router.controllers.postSearch, page.$route.current.params)
      console.log("prev results", results)
      console.log("query", $scope.query)
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
      console.log("RES:", res)
      $scope.partyResults = $scope.getResults("party", res)
      $scope.volumeResults = $scope.getResults("volume", res)

      $scope.partyCount = $scope.getTypeCounts("party", res)
      $scope.volumeCount = $scope.getTypeCounts("volume", res)
      console.log("PARTY RESULTS", $scope.partyResults)

      if $scope.partyCount
        $scope.affiliations = (c.party_affiliation_s for c in $scope.partyResults.docs)
      else
        $scope.affiliations = ["No people found"]

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
      else
        $scope.next = undefined
      if $scope.offset > 0
        $scope.prev = ->
          $scope.offset = Math.max(0, $scope.offset - $scope.limit)
          $scope.search()
      else
        $scope.prev = undefined

      $scope.updateFilterBoxOptions()

      $scope.number = 1 + ($scope.offset / $scope.limit)
      console.log("NUMBER: ", $scope.number, $scope.offset, $scope.limit)


    $scope.findFacet = (typeName, res) ->
      if res.facets.content_type
         facets = (facet.val for facet in res.facets.content_type.buckets)
         return facets.indexOf(typeName)
      else
         return -1

    $scope.getTypeCounts = (type, res) ->
      idx = $scope.findFacet(type, res)
      console.log(idx)
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

    $scope.getPartyFeatureBoxOpts = ->
      opts = [$scope.selectSessionStr, $scope.selectHighlightStr]
      return opts

    $scope.updateFilterBoxOptions = ->
      if "Volumes" in $scope.selectedType
        $scope.display = (s for s in $scope.getVolumeFeatureBoxOpts())
      if "People" in $scope.selectedType
        console.log("AFFILIATIONS:", $scope.affiliations)
        $scope.display = _.sortBy(_.uniq(a for a in $scope.affiliations))

    $scope.partyVolBoxClick = ->
      if "Volumes" in $scope.selectedType
        $scope.query = $scope.originalQuery
        $scope.search()
      if "People" in $scope.selectedType
        $scope.query = $scope.originalQuery
        $scope.search()
      console.log($scope.selectedType, $scope.display)

    $scope.filterBoxClick = ->
      console.log("FILTER BOX CLICKED", $scope.selectedFilter)
      if "Volumes" in $scope.selectedType
        if $scope.selectSessionStr in $scope.selectedFilter
          $scope.filterBySession()
        if $scope.selectHighlightStr in $scope.selectedFilter
          $scope.filterByHighlight()
      if "People" in $scope.selectedType
        $scope.partyFilterBoxClick()
        console.log("FILTERING BY PARTY")
      $scope.search()

    $scope.partyFilterBoxClick = ->
      $scope.filterByAffiliation()

    $scope.filterByAffiliation = ->
      $scope.query = $scope.originalQuery + "|arg=party_affiliation_s:\"" + $scope.selectedFilter + "\""

      # funcs = {"session" : filterBySession, "highlight" : $scope.filterByHighlight}

    # Now we want to rerun the search but only return vols w/ highlights
    $scope.filterByHighlight = ->
      $scope.requireHighlight = true
      $scope.query = $scope.originalQuery + "|arg=volume_has_excerpt_b:true"

    $scope.filterBySession = ->
      $scope.requireSession = true
      $scope.query = $scope.originalQuery + "|arg=volume_has_sessions_b:true"


    params = $location.search()
    $scope.query = params.query
    $scope.originalQuery = params.query
    console.log(results)
    $scope.offset = parseInt($location.search().offset, 10) || 0
    $scope.parseResults(results)

    console.log(results)


    return
]
