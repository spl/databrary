'use strict'

app.controller 'site/search', [
  '$scope', '$location', 'constantService', 'displayService', 'searchService', 'parties', 'volumes',
  ($scope, $location, constants, display, Search, parties, volumes) ->
    display.title = 'Search'
    $scope.constants = constants

    spellcheck = {}
    process = (r) ->
      list = r.response.docs
      list.count = r.response.numFound
      if (sug = r.spellcheck) && (sug = sug.suggestions)
        for i in [1..sug.length] by 2
          spellcheck[sug[i-1]] = sug[i].suggestion
          $scope.spellcheck = spellcheck
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
    if type
      offset = parseInt(params.offset, 10) || 0
      $scope.page = 1 + (offset / type.limit)
      $scope.pages = Math.ceil($scope.count / type.limit)

    parseNumber = (x) ->
      if x != '*' then parseFloat(x)
    parseRange = (x) ->
      if x = /^\[([-+0-9.e]+|\*) TO ([-+0-9.e]+|\*)\]$/i.exec(x)
        [parseNumber(x[1]), parseNumber(x[2])]
      else
        []
    printRange = (x) ->
      x && if x[0]? || x[1]? then '['+(x[0] ? '*')+' TO '+(x[1] ? '*')+']'
    printAge = (x) ->
      x[0] = undefined if x[0] <= 0
      x[1] = undefined if x[1] >= constants.age.limit
      printRange(x)

    $scope.fields = fields = {record_age:[]}
    $scope.metrics = metrics = {}
    for f, v of params
      if f.startsWith('f.')
        n = f.substr(2)
        fields[n] = if n == 'record_age' then parseRange(v) else v
      else if f.startsWith('m.')
        mi = f.substr(2)
        metrics[mi] = if constants.metric[mi].type == 'numeric' then parseRange(v) else v
    unless fields.record_age[0] > 0
      fields.record_age[0] = 0
    unless fields.record_age[1] < constants.age.limit
      fields.record_age[1] = constants.age.limit

    $scope.search = (offset) ->
      if !$scope.query && !offset && $.isEmptyObject(fields) && $.isEmptyObject(metrics)
        $location.replace().search({})
        return
      $location.replace()
        .search('q', $scope.query)
        .search('volume', type?.volume)
        .search('offset', offset || undefined)
      for f, v of fields
        $location.search('f.'+f,
          if f == 'record_age' then printAge(v) else v)
      for f, v of metrics
        $location.search('m.'+f,
          if constants.metric[f].type == 'numeric' then printRange(v) else v)
      return

    $scope.searchParties = (auth, inst) ->
      fields.party_authorization = auth
      fields.party_is_institution = inst
      type = Search.Party
      $scope.search()
    $scope.searchVolumes = () ->
      type = Search.Volume
      $scope.search()
    $scope.searchSpellcheck = (w, s) ->
      $scope.query = $scope.query.replace(w, s)
      $scope.search()
    $scope.searchPage = (n) ->
      $scope.search(type.limit*(n-1))

    limits =
      year: Math.ceil(constants.age.limit)
      month: Math.floor(5*constants.age.year)
      day: Math.floor(3*constants.age.month)
    $scope.ageMax = () ->
      m = fields.record_age[1]
      limits[display.age] ||
        if m < limits.day then limits.day
        else if m < limits.month then limits.month
        else limits.year

    $scope.availableMetrics = () ->
      m for m in constants.metrics when !metrics[m.id]? && m.release >= constants.permission.PUBLIC
    $scope.addMetric = () ->
      if mi = $scope.addMetric.id
        $scope.addMetric.id = ''
        metrics[mi] = if constants.metric[mi].type == 'numeric' then [] else undefined
      return
    $scope.removeMetric = (mi) ->
      metrics[mi] = null
      return

    $scope.optionsCompleter = (input, options) ->
      i = input.toLowerCase()
      match = (o for o in options when o.toLowerCase().startsWith(i))
      switch match.length
        when 0 then input
        when 1 then match[0]
        else ({text:o, select:o, default:input && i==0} for o, i in match)

    return
]
