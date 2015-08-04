'use strict'

app.controller 'site/search', [
  '$scope', '$location', 'displayService', 'results', 'pageService', 'solrModelService',
  ($scope, $location, display, results, page, solrModel) ->
    console.log("results")
    console.log(results)

  ###########################
  # Some constants
  ###########################
    $scope.selectedType = ""
    $scope.selectedVolume = ""
    $scope.selectedFilter = ""

    # Array that holds the names of the types the user
    # can filter by (volume, party)
    $scope.typeDisplay = []

    # Array that holds the current available filters
    # for the selected type.
    $scope.filterDisplay = []

    # These hold the current number of returned values
    # for each type. These are all set in parseResults.
    $scope.volumeCount = 0
    $scope.partyCount = 0
    $scope.totalCount = 0

    $scope.selectSessionStr = "Volumes w/ Sessions"
    $scope.selectHighlightStr = "Volumes w/ Highlights"
    $scope.selectPeopleFilters = ["All", "Authorized Investigators", "Institutions"]

    $scope.volumeDisplayStr = "Volumes"
    $scope.partyDisplayStr = "People"
    $scope.partyLinkPrefix = "party/"
    $scope.volumeLinkPrefix = "volume/"

    # Holds the current text of the search box
    $scope.searchBoxQuery = ""

    # List of affiliations (currently unused)
    $scope.affiliations = []

    # Map for volid -> containers, fill in has requested
    $scope.retrievedContainers = {}

    # Number of results of each facet to return.
    # Currently hardcoded to 10.
    $scope.limit = 10
    display.title = 'Search'

    # Name of the currently selected filter.
    currentFilter = ""

    ageRangeMin = 0
    ageRangeMax = 0

    #


  ###########################
  # Functions for transforming a document into strings
  ###########################

    # Def formPartyLink:
    # Given a party document, returns a hyperlink.
    formPartyLink = (doc) ->
      return partyLinkPrefix + doc.party_id_i

    # Def formVolumeLink:
    # Function that, given a volume document, returns a hyperlink.
    formVolumeLink = (doc) ->
      return volumeLinkPrefix + doc.volume_id_i

    # Def formVolumeResult:
    # This is a helper function that concats parts of a volume into a string.
    $scope.formVolumeResult = (volumeModel) ->
      # Form everything except the title, which we'll do in HTML so we can make it the link
      res = []
      # If the volumeModelument is a volume (and has an abstract) push it to the result list
      if volumeModel.body
       # Clip everything after the first 150 characters just for display purposes
       if $scope.retrievedContainers[volumeModel.id] == undefined && volumeModel.body.length > 150
          temp = volumeModel.body[0..150] + "..."
       else
          temp = volumeModel.body
       res.push temp
      # If the volumeModelument is a volume (and has a citation) also push it to the result list
      if volumeModel.citation
        res.push volumeModel.citation
      # Add more stuff here if we want it
      return res.join("\n")

  ###########################
  # Search handlers
  ###########################
    clearAllContainers = ->
      $scope.retrievedContainers = {}

    $scope.clearContainers = (vol) ->
      $scope.retrievedContainers[vol.id] = undefined

    $scope.searchContainers = (vol) ->
      console.log("Getting container results for volume:", vol.id)
      beforeQuery = $scope.query
      query = $scope.query + "|type=container"
      query = searchVolume(vol, query)
      # query = $scope.query + "|type=container|arg=container_volume_id_i:" + "15"
      $scope.search(query, vol.id)
      $scope.query = beforeQuery


    # Def searchBox: read the current text in the search box
    # and initiate a search. If blank, return all results.
    $scope.searchBox = ->
      console.log("The search box was :", $scope.searchBoxQuery)
      console.log("The original query was :", $scope.originalQuery)
      if $scope.searchBoxQuery? and $scope.searchBoxQuery != ""
        $scope.originalQuery = $scope.searchBoxQuery
      else
        $scope.originalQuery = "*"
      $scope.query = $scope.originalQuery
      $scope.offset = 0
      console.log("NEW SEARCH:", $scope.query)
      $scope.search()

    # Def search: the function that is actually calling the search from Haskell.
    # Get the results from Solr and pass them into parseResults.
    $scope.search = (query = "", volId = -100) ->
      # This is for if we want to pass an argument into this...
      if query.length > 0 and volId < 0
        $scope.query = query
        $scope.originalQuery = query
      else if query.length > 0
        $scope.query = query

      clearAllContainers()

      # Set the currently selected filter options here
      if ageRangeMin > 0 or ageRangeMax > 0
        if volId > 0
          $scope.query = searchAge($scope.query, ageRangeMin, ageRangeMax, false)
        else
          $scope.query = searchAge($scope.query, ageRangeMin, ageRangeMax)


      console.log("QUERY IS: ", $scope.query)

      promise = page.router.http(page.router.controllers.postSearch,
        {"query" : $scope.query, "offset" : $scope.offset, "limit" : $scope.limit})
      promise.then (res) ->
        console.log("GOT RES:", res)
        if volId < 0
          parseResults(res.data)
        else
          parseContainerResults(res.data, volId)

    # This function will return the larger number of results of the
    # two types. So if there are more volumes than parties, it returns
    # the number of volumes.
    $scope.maxResults = ->
      return Math.max($scope.volumeCount, $scope.partyCount)


    parseContainerResults = (res, volId) ->
      console.log("CONTAINTER RESULTS", res, volId)
      models = createModels(res)
      console.log(models)
      containers = models.records
      numContainers = getTypeCounts("record", res)
      $scope.retrievedContainers[volId] = containers
      console.log("CONTAINERS:", containers, $scope.retrievedContainers)


    #################################
    # Parse results, the main workhorse function.
    # This function is called after each search and is used to set
    # all of the params for use in the html.
    # ##############################
    parseResults = (res) ->
      if res == "null"
        $scope.query = "*"
        $scope.search()
        return
      # if !$scope.query? or !$scope.query or $scope.query == "" or $scope.query == "null" or $scope.query == "false" or $scope.query == "undefined"
        # $scope.query = "*"
      console.log("query before anything:", $scope.query)
      console.log("RES:", res)
      models = createModels(res)
      $scope.partyModels = models.parties
      $scope.volumeModels = models.volumes

      # Set the number of parties and volumes returned.
      $scope.partyCount = getTypeCounts("party", res)
      $scope.volumeCount = getTypeCounts("volume", res)

      # If there is a suggested query, extract it.
      $scope.suggestedQuery = getSuggestedQuery(res)

      # Include the number of returned results in the type display.
      $scope.typeDisplay = [$scope.partyDisplayStr + " (" + ($scope.partyCount || 0) + ")",
        $scope.volumeDisplayStr + " (" + ($scope.volumeCount || 0) + ")"]

      console.log("SUGGESTED QUERY", $scope.suggestedQuery)
      $scope.totalCount = $scope.partyCount + $scope.volumeCount

      # Set up pagination
      $scope.minPage = 1
      $scope.maxPage = 1 + ($scope.maxResults() / ($scope.limit + 1))
      pageRange = []
      for i in [$scope.minPage .. $scope.maxPage] by 1
        pageRange.push(i)

      $scope.pageRange = pageRange

      $scope.goToPage = (page) ->
        $scope.offset = $scope.limit * (page-1)
        $scope.search()

      if parseInt($scope.maxResults()) > ($scope.offset + $scope.limit)
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

      updateFilterBoxOptions()

      $scope.number = 1 + ($scope.offset / $scope.limit)
      $scope.query = $scope.originalQuery

################################
# End parse results
# ##############################

################################
# Helper functions
################################

    findFacet = (typeName, res) ->
      if res != null and res.facets.content_type
         facets = (facet.val for facet in res.facets.content_type.buckets)
         return facets.indexOf(typeName)
      else
         return -1

    getTypeCounts = (type, res) ->
      idx = findFacet(type, res)
      console.log(idx)
      if idx < 0
        return null
      return res?.facets?.content_type?.buckets[idx].count ? 0

    findResult = (typeName, res) ->
      console.log("RESULT IS", res)
      if res != null
        groups = (group.groupValue for group in res.grouped.content_type.groups)
        return groups.indexOf(typeName)
      else
        return -1

    getCorrectedSpelling = (res) ->
      return res.spellcheck.suggestions

    getSuggestedQuery = (res) ->
      if res.spellcheck?.collations.length > 0
        return res.spellcheck.collations[1].collationQuery
      return null

    getResults = (type, res) ->
      idx = findResult(type, res)
      if idx < 0
        return null
      return res.grouped.content_type.groups[idx].doclist

    getVolumeFilterBoxOpts = ->
      opts = [$scope.selectSessionStr, $scope.selectHighlightStr]
      return opts

    getPartyFilterBoxOpts = ->
      opts = [$scope.selectSessionStr, $scope.selectHighlightStr]
      return opts

    updateFilterBoxOptions = ->
      console.log("SELTYPE", $scope.selectedType)
      if $scope.selectedType and currentFilter != $scope.selectedType
        # We have to reset offset
        $scope.offset = 0
        if $scope.selectedType.join(" ").includes($scope.volumeDisplayStr)
          $scope.filterDisplay = (s for s in getVolumeFilterBoxOpts())
        if $scope.selectedType.join(" ").includes($scope.partyDisplayStr)
          # console.log("AFFILIATIONS:", $scope.affiliations)
          $scope.filterDisplay = (s for s in $scope.selectPeopleFilters)
        currentFilter = $scope.selectedType

    # Takes a group of documents, an argument name, and a value, then counts
    # how many documents in the set have that value
    countWithArg = (group, argument, value) ->
      console.log(group)
      result = (d for d in group.docs when d[argument] == value).length || 0
      return result

    # Perform a new search for people or volumes only... NOTE: does not do anything yet
    # TODO Either make this do something useful or remove the search part of it
    $scope.partyVolBoxClick_temp = (t) ->
      if t.includes($scope.volumeDisplayStr) || t.includes($scope.partyDisplayStr)
        $scope.query = $scope.originalQuery
        $scope.search()
      console.log(t, $scope.filterDisplay)

    $scope.partyVolBoxClick = ->
      console.log($scope.selectedType, $scope.selectedType.join(" "))
      if $scope.selectedType.join(" ").includes($scope.volumeDisplayStr) || $scope.selectedType.join(" ").includes($scope.partyDisplayStr)
        $scope.query = $scope.originalQuery
        $scope.search()
      console.log($scope.selectedType, $scope.filterDisplay)

    # Action to do something when a filter option is clicked
    $scope.filterBoxClick = ->
      console.log("FILTER BOX CLICKED", $scope.selectedFilter)
      if $scope.selectedType.join(" ").includes($scope.volumeDisplayStr)
        if $scope.selectSessionStr in $scope.selectedFilter
          filterBySession()
        if $scope.selectHighlightStr in $scope.selectedFilter
          filterByHighlight()
      if $scope.selectedType.join(" ").includes($scope.partyDisplayStr)
        # $scope.partyFilterBoxClick()
        filterParties($scope.selectedFilter)
        console.log("FILTERING BY PARTY")
      $scope.offset = 0 # Reset the offset
      $scope.search()

    addArgToQuery = (query, arg, val) ->
      return query + "|arg=" + arg + ":\"" + val + "\""


    $scope.partyFilterBoxClick = ->
      filterByAffiliation()

    filterByAffiliation = ->
      $scope.query = $scope.originalQuery + "|arg=party_affiliation_s:\"" + $scope.selectedFilter + "\""

      # funcs = {"session" : filterBySession, "highlight" : filterByHighlight}

    # Now we want to rerun the search but only return vols w/ highlights
    filterByHighlight = ->
      $scope.requireHighlight = true
      $scope.query = $scope.originalQuery + "|arg=volume_has_excerpt_b:true"

    filterBySession = ->
      $scope.requireSession = true
      $scope.query = $scope.originalQuery + "|arg=volume_has_sessions_b:true"

    filterParties = (filterName) ->
      console.log("Filtering parties by", filterName)
      if filterName.join(" ").includes("All")
        # Select all parties
        $scope.query = $scope.originalQuery # TODO should this do something else?
      else if filterName.join(" ").includes("Institution")
        # Select institutions
        $scope.query = addArgToQuery($scope.originalQuery, "party_is_institution_b", "true")
      else if filterName.join(" ").includes("Authorized")
        # Select authorized users only
        $scope.query = addArgToQuery($scope.originalQuery, "party_is_authorized_b", "true")
      console.log("Query after filter", $scope.query)

    createModels = (res) ->
      parties = getResults("party", res)
      volumes = getResults("volume", res)
      slots = getResults("record", res)
      partyModels = if parties?.docs then (new solrModel.SolrParty(p) for p in parties.docs) else []
      volumeModels = if volumes?.docs then (new solrModel.SolrVolume(v) for v in volumes.docs) else []
      recordModels = if slots?.docs then (new solrModel.SolrSlot(s) for s in slots.docs) else []
      console.log("PARTY TIME", parties, partyModels)
      console.log("VOLUME TIME", volumes, volumeModels)
      console.log("RECORD TIME", slots, recordModels)
      return { parties: partyModels, volumes: volumeModels, records: recordModels }

    searchVolume = (volume, query) ->
      arg = "|arg=volume_id_i:#{ volume.id }"
      return query + arg

    searchAge = (query, ageMin, ageMax, join=true) ->
      # arg = "|arg=record_age_td:[#{ ageMin } TO #{ ageMax }]"
      if join
        arg = "|join=volume_id_i,volume_id_i,record_age_td:[#{ ageMin } TO #{ ageMax }]"
      else
        arg = "|arg=record_age_td:[#{ ageMin } TO #{ ageMax }]"
      return query + arg

    searchSex = (volume, query, sex) ->
      arg = "|arg=record_gender_s:[#{ sex }]"
      return query + arg

    searchSetting = (volume, query, setting) ->
      arg = "|arg=record_setting_s:[#{ setting }]"
      return query + arg

    searchRace = (volume, query, race) ->
      arg = "|arg=record_race_s:[#{ race }]"
      return query + arg

    # searchTags = (query, tag, user="") ->
      # query = searchVolume(volume, query)
      # arg = "|arg=record_tag_s:[#{ ageMin } TO #{ ageMax }]"
      # return query + arg

    # age range slider wip
    $ ->
      $('#age-slider').slider
        range: true
        min: 0
        max: 500
        slide: (event, ui) ->
          $('#age').val ui.values[0] + ' - ' + ui.values[1] + ' days'
          return
        stop: (event, ui) ->
          $scope.updateAgeRange()
          return
      $('#age').val $('#age-slider').slider('values', 0) + ' - ' + $('#age-slider').slider('values', 1) + ' days'
      return

    $scope.updateAgeRange = ->
      ageRangeMin = $('#age-slider').slider('values', 0)
      ageRangeMax = $('#age-slider').slider('values', 1)
      console.log ageRangeMin, ageRangeMax
      $scope.search()
      return

    # Code for the initial loado
    params = $location.search()
    $scope.query = params.query
    console.log("INITIAL QUERY:", $scope.query)
    if !params.query?
      $scope.originalQuery = "*"
    else
      $scope.originalQuery = params.query
    $scope.searchBoxQuery = $scope.originalQuery
    console.log("results")
    $scope.offset = parseInt($location.search().offset, 10) || 0
    parseResults(results)

    console.log results
    return
]
