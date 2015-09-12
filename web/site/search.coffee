'use strict'

app.controller 'site/search', [
  '$scope', '$location', 'constantService', 'displayService', 'modelService', 'searchService', 'parties', 'volumes',
  ($scope, $location, constants, display, models, Search, parties, volumes) ->
    display.title = 'Search'

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
      $scope.pageCurrent = 1 + (offset / type.limit)
      $scope.pageCount = Math.ceil($scope.count / type.limit)

    finite = (x) ->
      x? && isFinite(x)
    parseNumber = (x) ->
      if x != '*' then parseFloat(x)
    printNumber = (x) ->
      if finite(x) then x else '*'
    parseRange = (x) ->
      x = /^\[([-+0-9.e]+|\*) TO ([-+0-9.e]+|\*)\]$/i.exec(x) || []
      [parseNumber(x[1]) ? -Infinity, parseNumber(x[2]) ? Infinity]
    printRange = (x) ->
      x && if finite(x[0]) || finite(x[1]) then '['+printNumber(x[0])+' TO '+printNumber(x[1])+']'
    printAge = (x) ->
      printRange([(if x[0] > 0 then x[0]), (if x[1] < constants.age.limit then x[1])])

    $scope.fields = fields = {record_age:[-Infinity, Infinity]}
    $scope.metrics = metrics = {}
    for f, v of params
      if f.startsWith('f.')
        n = f.substr(2)
        fields[n] = if n == 'record_age' || n == 'container_date' then parseRange(v) else v
      else if f.startsWith('m.')
        mi = f.substr(2)
        metrics[mi] = if constants.metric[mi].type == 'numeric' then parseRange(v) else v
    if fields.container_date && (finite(fields.container_date[0]) || finite(fields.container_date[1]))
      fields.container_top = 'false'

    any = (o) ->
      for f, v of o
        return true if v?
      return false

    $scope.search = (offset) ->
      q =
        q: $scope.query || undefined
        offset: offset
      delete fields.container_date unless fields.container_top
      for f, v of fields
        q['f.'+f] = if f == 'record_age' then printAge(v) else if f == 'container_date' then printRange(v) else v || undefined
      delete q['f.container_top'] if q['f.container_date']
      for f, v of metrics
        q['m.'+f] = if constants.metric[f].type == 'numeric' then printRange(v) else v || undefined
      if any(q)
        q.volume = type?.volume
      $location.search(q)
      return

    $scope.searchParties = (auth, inst) ->
      fields.party_authorization = auth
      fields.party_is_institution = inst
      type = Search.Party
      $scope.search(0)
    $scope.searchVolumes = () ->
      type = Search.Volume
      $scope.search(0)
    $scope.searchSpellcheck = (w, s) ->
      $scope.query = $scope.query.replace(w, s)
      $scope.search()
    $scope.searchPage = (n) ->
      $scope.search(type.limit*(n-1))

    startTag = fields.tag_name
    $scope.tagSearch = (input) ->
      return input if input == startTag
      models.Tag.search(input).then (data) ->
        for tag in data
          text: tag
          select: tag

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
    $scope.clearAge = () ->
      fields.record_age = [-Infinity,Infinity]

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

    $scope.expandVolume = (v) ->
      $scope.expanded = {volume:v}
      return unless v
      v.search($location.search()).then (r) ->
        $scope.expanded.results = r
        return
      return

    return
]
