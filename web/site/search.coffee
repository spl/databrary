'use strict'

app.controller 'site/search', [
  '$scope', '$location', 'constantService', 'displayService', 'searchService', 'parties', 'volumes',
  ($scope, $location, constants, display, Search, parties, volumes) ->
    display.title = 'Search'
    $scope.constants = constants

    process = (r) ->
      list = r.response.docs
      list.count = r.response.numFound
      list

    if parties
      $scope.parties = process(parties)
      unless volumes
        type = Search.Party
        $scope.count = $scope.parties.count
    if volumes
      $scope.volumes = process(volumes)
      unless parties
        type = Search.Volume
        $scope.count = $scope.volumes.count

    params = $location.search()
    $scope.query = params.q || ''
    offset = parseInt(params.offset, 10) || 0
    if type
      $scope.page = 1 + (offset / type.limit)
      $scope.pages = Math.ceil($scope.count / type.limit)
    $scope.fields = fields = {}
    $scope.metrics = metrics = {}
    for f, v of params
      if f.startsWith('f.')
        fields[f.substr(2)] = v
      else if f.startsWith('m.')
        metrics[f.substr(2)] = v

    $scope.search = () ->
      if !$scope.query && !offset && $.isEmptyObject(fields) && $.isEmptyObject(metrics)
        $location.replace().search({})
        return
      $location.replace()
        .search('q', $scope.query)
        .search('volume', type?.volume)
        .search('offset', offset || undefined)
      for f, v of fields
        $location.search('f.'+f, v)
      for f, v of metrics
        $location.search('m.'+f, v)
      return

    $scope.searchParties = (auth, inst) ->
      fields.party_authorization = auth
      fields.party_is_institution = inst
      type = Search.Party
      $scope.search()
    $scope.searchVolumes = () ->
      type = Search.Volume
      $scope.search()
    $scope.searchPage = (n) ->
      offset = type.limit*(n-1)
      $scope.search()

    return


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

    days = {display:"days",div:1,max:500}
    months = {display:"months",div:30,max:60}
    years = {display:"years",div:365,max:92}
    $scope.ageUnits = [days,months,years]
    $scope.selectedAgeUnit.div = days.div
    ageRangeMin = 0
    ageRangeMax = days.max
    ageRangeLower = ageRangeMin
    ageRangeUpper = ageRangeMax

    highlightFilter = false
    sessionFilter = false
    partyFilter = false
    volumeFilter = false

    $scope.allMetrics = (mv for mi,mv of page.constants.metric when mv.release)
    $scope.allMetrics.sort((a,b) -> a.id - b.id)
    console.log $scope.allMetrics
    $scope.selectedMetrics = []

    # metricToFunction = {
      # "id" : searchId,
      # "reason" : searchReason,
      # "state" : searchState,
      # "gestational age (weeks)" : searchAge,
      # "summary" : searchSummary,
      # "race" : searchRace,
      # "ethnicity" : searchEthnicity,
      # "language" : searchLanguage,
      # "country" : searchCountry,
      # "info" : searchInfo,
      # "description" : searchDescription
    # }



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

    $scope.resetFilters = ->
      highlightFilter = false
      sessionFilter = false
      partyFilter = false
      volumeFilter = false
      $scope.selectedFilter = ""
      $scope.selectedType = ""
      $scope.selectedVolume = ""
      $scope.filterDisplay = []
      for m in $scope.selectedMetrics
        $scope.returnMetric(m)
      ageRangeLower = ageRangeMin
      ageRangeUpper = ageRangeMax
      $scope.search()

    $scope.clearContainers = (vol) ->
      $scope.retrievedContainers[vol.id] = undefined

    $scope.searchContainers = (vol) ->
      console.log("Getting container results for volume:", vol.id)
      beforeQuery = $scope.query
      query = $scope.query + "|type=container"
      query = searchVolume(vol, query)
      $scope.search(query, vol.id)
      $scope.query = beforeQuery


    # Def searchBox: read the current text in the search box
    # and initiate a search. If blank, return all results.
    $scope.searchBox = ->
      console.log("The search box was :", $scope.searchBoxQuery)
      console.log("The original query was :", $scope.originalQuery)

      # Handle user clearing search box and hitting enter
      if $scope.searchBoxQuery? and $scope.searchBoxQuery != ""
        $scope.originalQuery = $scope.searchBoxQuery
      else
        $scope.originalQuery = "*"

      $scope.resetFilters()

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
        $scope.searchBoxQuery = query
      else if query.length > 0
        $scope.query = query

      if $scope.searchBoxQuery? and $scope.searchBoxQuery.length > 0
        if $location.search()["debug"]?
          $location.search("query=" + $scope.searchBoxQuery + "&debug=1")
        else
          $location.search("query=" + $scope.searchBoxQuery)
      else
        $location.search("")

      if !$scope.query?
        $scope.query = "*"

      clearAllContainers()

      # Set the currently selected filter options here
      if ageRangeLower > 0 or ageRangeUpper > 0
        if volId > 0
          $scope.query = searchAge($scope.query, ageRangeLower, ageRangeUpper, false)
        else
          $scope.query = searchAge($scope.query, ageRangeLower, ageRangeUpper)

      if sessionFilter
        $scope.query = searchSession($scope.query)

      if highlightFilter
        $scope.query = searchHighlight($scope.query)

      if partyFilter
        $scope.query = searchParties($scope.query)

      if volumeFilter
        $scope.query = searchVolumes($scope.query)

      for m in $scope.selectedMetrics
        # if m.value?
          $scope.query = searchMetric(m, $scope.query, m.value)
      # Party search?


      # Send the search
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
      # if res == "null" or !res?
        # $scope.query = "*"
        # $scope.search()
        # return

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
        if $scope.selectedType.includes($scope.volumeDisplayStr)
          $scope.filterDisplay = (s for s in getVolumeFilterBoxOpts())
        if $scope.selectedType.includes($scope.partyDisplayStr)
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
      $scope.selectedType = t
      if t.includes($scope.volumeDisplayStr)
        volumeFilter = true
      else
        volumeFilter = false
      if t.includes($scope.partyDisplayStr)
        partyFilter = true
      else
        partyFilter = false
      $scope.search()
      console.log(t, $scope.filterDisplay)

    $scope.partyVolBoxClick = ->
      console.log($scope.selectedType, $scope.selectedType.join(" "))
      if $scope.selectedType.join(" ").includes($scope.volumeDisplayStr)
        volumeFilter = true
      else
        volumeFilter = false
      if $scope.selectedType.join(" ").includes($scope.partyDisplayStr)
        partyFilter = true
      else
        partyFilter = false
      $scope.search()
      console.log($scope.selectedType, $scope.filterDisplay)

    # Action to do something when a filter option is clicked
    $scope.filterBoxClick_temp = (s) ->
      console.log("FILTER BOX CLICKED", $scope.selectedFilter)
      if s.includes($scope.volumeDisplayStr)
        if s.includes($scope.selectSessionStr)
          sessionFilter = true
        else
          sessionFilter = false
        if s.includes($scope.selectHighlightStr)
          highlightFilter = true
        else
          highlightFilter = false
      if s.includes($scope.partyDisplayStr)
        partyFilter = true
        console.log("FILTERING BY PARTY")
      else
        partyFilter = false
      $scope.offset = 0 # Reset the offset
      $scope.search()

    $scope.filterBoxClick = ->
      console.log("FILTER BOX CLICKED", $scope.selectedFilter)
      if $scope.selectedType.join(" ").includes($scope.volumeDisplayStr)
        if $scope.selectSessionStr in $scope.selectedFilter
          sessionFilter = true
        else
          sessionFilter = false
        if $scope.selectHighlightStr in $scope.selectedFilter
          highlightFilter = true
        else
          highlightFilter = false
      if $scope.selectedType.join(" ").includes($scope.partyDisplayStr)
        partyFilter = true
        console.log("FILTERING BY PARTY")
      else
        partyFilter = false
      $scope.offset = 0 # Reset the offset
      $scope.search()

    addArgToQuery = (query, arg, val) ->
      return query + "|arg=" + arg + ":\"" + val + "\""

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

    searchAffiliation = (query) ->
      return addArgToQuery(query, "party_affiliation_s", $scope.selectedFilter)

    # Now we want to rerun the search but only return vols w/ highlights
    searchHighlight = (query) ->
      $scope.requireHighlight = true
      return addArgToQuery(query, "has_excerpt_b", "true")

    searchSession = (query) ->
      $scope.requireSession = true
      return addArgToQuery(query, "volume_has_sessions_b", "true")

    searchVolumes = (query) ->
      return addArgToQuery(query, "content_type", "volume")

    searchParty = (query) ->
      return addArgToQuery(query, "content_type", "party")

    searchParties = (query) ->
      filterName = $scope.selectedFilter
      console.log("Filtering parties by", filterName)
      if filterName? and filterName.length > 0
        if filterName.join(" ").includes("All")
          # Select all parties
          query = searchParty(query)
        else if filterName.join(" ").includes("Institution")
          # Select institutions
          query = addArgToQuery(query, "party_is_institution_b", "true")
        else if filterName.join(" ").includes("Authorized")
          # Select authorized users only
          query = addArgToQuery(query, "party_is_authorized_b", "true")
      else
        query = searchParty(query)
      return query

    searchVolume = (volume, query) ->
      arg = "|arg=volume_id_i:#{ volume.id }"
      return query + arg

    searchAge = (query, ageMin, ageMax, join=true) ->
      # arg = "|arg=record_age_ti:[#{ ageMin } TO #{ ageMax }]"
      if join
        arg = "|join=volume_id_i,volume_id_i,record_age_ti:[#{ ageMin } TO #{ ageMax }]"
      else
        arg = "|arg=record_age_ti:[#{ ageMin } TO #{ ageMax }]"
      return query + arg

    badFieldName = /\W/g
    searchMetric = (metricObj, query, value) ->
      suffix = selectSuffix(metricObj.metric)
      metricName = metricObj.metric.name.replace(badFieldName, '_')
      arg = "|arg=record_#{ metricName }#{ suffix }:\"#{ value }\""
      console.log("Adding arg:", metricName, arg)
      return query + arg

    selectSuffix = (metric) ->
      console.log("SWITCHING: ", metric, metric.type)
      suffix = switch metric.type
        when "text"
          if metric.options
            "_s"
          else
            "_t"
        when "numeric" then "_td"
        when "date" then "_tdt"
        else ""
      return suffix


    # searchTags = (query, tag, user="") ->
      # query = searchVolume(volume, query)
      # arg = "|arg=record_tag_s:[#{ ageMin } TO #{ ageMax }]"
      # return query + arg

    # age range slider wip
    $ ->
      $('#age-slider').slider
        range: true
        min: ageRangeMin
        max: ageRangeMax
        values: [ageRangeMin,ageRangeMax]
        slide: (event, ui) ->
          $('#ageLower').val ui.values[0]
          $('#ageUpper').val ui.values[1]
          return
        stop: (event, ui) ->
          $scope.updateAgeRange()
          return
      $('#ageLower').val $('#age-slider').slider('values', 0)
      $('#ageUpper').val $('#age-slider').slider('values', 1)
      return

    $scope.updateAgeRange = ->
      ageRangeLower = $('#age-slider').slider('values', 0)
      ageRangeUpper = $('#age-slider').slider('values', 1)
      $scope.search()
      return

    $scope.$on('$routeUpdate',() ->
      $scope.query = $location.search().query
      $scope.searchBoxQuery = $scope.query
      $scope.search()
    )


# other metrics filters
    optionCompletions = (input,options) ->
      i = input.toLowerCase()
      (o for o in options when o.toLowerCase().startsWith(i))

    $scope.optionsCompleter = (input,options) ->
      match = optionCompletions(input,options)
      switch match.length
        when 0
          input
        when 1
          match[0]
        else
          ({text:o, select:o, default: input && i==0} for o, i in match)


    $scope.updateSelectedMetrics = ->
      $scope.selectedMetrics.push({metric:$scope.selectedMetric})
      rmindex = $scope.allMetrics.remove($scope.selectedMetric)
      console.log($scope.selectedMetrics)
      $scope.selectedMetric = undefined
      return

    $scope.returnMetric = (metric) ->
      rmindex = $scope.selectedMetrics.remove(metric)
      $scope.allMetrics.push(metric.metric)
      $scope.allMetrics.sort((a,b) -> a.id - b.id)
      $scope.search()
      return

    # Code for the initial load
    params = $location.search()
    $scope.query = params.query
    console.log("INITIAL QUERY:", $scope.query)
    if params.query?
      $scope.originalQuery = params.query
    $scope.searchBoxQuery = $scope.originalQuery
    console.log("results")
    $scope.offset = parseInt($location.search().offset, 10) || 0
    parseResults(results)

    console.log results
    return
]
