'use strict'

app.controller('volume/slot', [
  '$scope', '$location', '$sce', '$timeout', 'constantService', 'displayService', 'messageService', 'tooltipService', 'styleService', 'storageService', 'Offset', 'Segment', 'uploadService', 'routerService', 'slot', 'edit',
  ($scope, $location, $sce, $timeout, constants, display, messages, tooltips, styles, storage, Offset, Segment, uploads, router, slot, editing) ->
    display.title = slot.displayName
    $scope.flowOptions = uploads.flowOptions
    $scope.slot = slot
    $scope.volume = slot.volume
    $scope.editing = editing # $scope.editing (but not editing) is also a modal (toolbar) indicator
    $scope.form = {} # all ng-forms are put here
    $flow = $scope.$flow # not really in this scope

    ################################### Base classes/utilities

    # Represents a point on the timeline, which can be expressed as "o" (offset time in ms), "x" (pixel X position in client coordinates), or "p" (fractional position on the timeline, may escape [0,1])
    class TimePoint
      constructor: (v, t) ->
        if v instanceof TimePoint
          for t, x of v when t.startsWith('_')
            this[t] = x
        else
          this['_'+(t ? 'o')] = v

      tl = document.getElementById('slot-timeline')

      Object.defineProperties @prototype,
        o:
          get: ->
            if '_o' of @
              @_o
            else
              p = @p
              @_o =
                Math.round(ruler.range.l + p * (ruler.range.u - ruler.range.l))
          set: (o) ->
            return if o == @_o
            @_o = o
            delete @_p
            delete @_x
            return
        p:
          get: ->
            if '_p' of @
              @_p
            else if '_o' of @
              o = @_o
              @_p = (o - ruler.range.base) / (ruler.range.u - ruler.range.l)
            else if '_x' of @
              x = @_x
              tlr = tl.getBoundingClientRect()
              @_p = (x - tlr.left) / tlr.width
          set: (p) ->
            return if p == @_p
            @_p = p
            delete @_o
            delete @_x
            return
        x:
          get: ->
            if '_x' of @
              @_x
            else
              p = @p
              tlr = tl.getBoundingClientRect()
              @_x = tlr.left + p * tlr.width
          set: (x) ->
            return if x == @_x
            @_x = x
            delete @_o
            delete @_p
            return

      reset: ->
        if '_o' of @
          delete @_p
          delete @_x
        else if '_x' of @
          delete @_p
        return

      clip: ->
        p = @p
        if p > 1
          @p = Infinity
        if p < 0
          @p = -Infinity
        this

      defined: ->
        isFinite(@o)

      minus: (t) ->
        r = new TimePoint()
        if '_o' of this
          r.o = this._o - t.o
        if '_p' of this
          r.p = this._p - t.p
        if '_x' of this
          r.x = this._x - t.x
        r

      seek: ->
        if isFinite(o = @o)
          seekOffset(o)
          unless ruler.selection.contains(o) || ruler.selection.u == o # "loose" contains
            ruler.selection = new TimeSegment(null)
            finalizeSelection()
        return

      style: ->
        style = {}
        p = @p
        if p >= 0 && p <= 1
          style.left = 100*p + '%'
        style

    # Elaboration on Segment that uses lt and ut TimePoints
    class TimeSegment extends Segment
      init: (a, u) ->
        if arguments.length >= 2
          @lt = new TimePoint(a)
          @ut = new TimePoint(u)
        else if a instanceof TimeSegment
          @lt = new TimePoint(a.lt)
          @ut = new TimePoint(a.ut)
        else if a instanceof TimePoint
          @lt = new TimePoint(a)
          @ut = new TimePoint(a)
        else
          @lt = new TimePoint()
          @ut = new TimePoint()
          super

      Object.defineProperties @prototype,
        l:
          get: ->
            @lt.o
          set: (o) ->
            @lt.o = o
            return
        u:
          get: ->
            @ut.o
          set: (o) ->
            @ut.o = o
            return
        size:
          get: ->
            @ut.minus(@lt)

      contains: (t) ->
        if t instanceof TimePoint
          t = t.o
        super(t)

      reset: ->
        @lt.reset()
        @ut.reset()

      style: ->
        style = {}
        l = @lt.p
        r = @ut.p
        if l < 0
          style.left = '0px'
          style['border-top-left-radius'] = '0px'
          style['border-bottom-left-radius'] = '0px'
          if r < 0
            style.border = '#f26363 3px solid'
          else
            style['border-left'] = '0px'
        else if l < 1
          style.left = 100*l + '%'
        if r > 1
          style.right = '0px'
          style['border-top-right-radius'] = '0px'
          style['border-bottom-right-radius'] = '0px'
          if l > 1
            style.border = '#f26363 3px solid'
          else
            style['border-right'] = '0px'
        else if r > 0
          style.right = 100*(1-r) + '%'
        style

      select: (event) ->
        return false if $scope.editing == 'position'
        ruler.selection = new TimeSegment(if @full then null else @)
        if isFinite(@l) && !@contains(ruler.position)
          seekOffset(@l)
        else if @full
          seekOffset(undefined)
        finalizeSelection()
        event?.stopPropagation()

    ################################### Global state

    target = $location.search()
    video = undefined # currently playing video element
    blank = undefined # blank asset track for uploading
    fullRange = new Segment(0, 0) # total computed (asset + record) extent of this container (for zoom out full)
    ruler = $scope.ruler =
      range: if 'range' of target then new Segment(target.range) else fullRange # currently displayed zoom range
      selection: new TimeSegment(if 'select' of target then target.select else null) # current temporal selection
      position: new TimePoint(Offset.parse(target.pos)) # current position, also asset seek point (should be within selection)
      zoomed: 'range' of target # are we currently zoomed in?

    searchLocation = (url) ->
      url
        .search('asset', undefined)
        .search('record', undefined)
        .search($scope.current?.type || '', $scope.current?.id)
        .search('select', if !ruler.selection.empty then ruler.selection.format())
        .search('range', if ruler.zoomed then ruler.range.format())

    $scope.toggleEdit = () ->
      searchLocation($location.url(if editing then slot.route() else slot.editRoute()))

    byId = (a, b) -> a.id - b.id
    byPosition = (a, b) -> a.l - b.l
    finite = (args...) -> args.find(isFinite)

    resetRange = () ->
      for c in [$scope.assets, records, $scope.consents, $scope.tags]
        for t in c
          t.reset()
      ruler.selection.reset()
      return

    updateRange = () ->
      l = Infinity
      u = -Infinity
      for c in [$scope.assets, records, $scope.consents]
        for t in c
          if isFinite(t.l)
            l = t.l if t.l < l
            u = t.l if t.l > u
          if isFinite(t.u)
            l = t.u if t.u < l
            u = t.u if t.u > u
      fullRange.l = finite(slot.segment.l, l, 0)
      fullRange.u = finite(slot.segment.u, u, 0)
      resetRange()
      return

    ################################### Video/playback controls

    $scope.updatePosition = () ->
      seg = (if $scope.editing == 'position' then $scope.current else $scope.asset?.segment)
      if seg && video && seg.contains(ruler.position.o) && seg.lBounded
        video[0].currentTime = (ruler.position.o - seg.l) / 1000
      return

    seekOffset = (o) ->
      ruler.position.o = Math.round(o)
      $scope.updatePosition()
      return

    $scope.play = ->
      if video
        video[0].play()
      else
        $scope.playing = 1
      return

    $scope.pause = ->
      if video
        video[0].pause()
      else
        $scope.playing = 0
      return

    videoEvents =
      pause: ->
        $scope.playing = 0
        return
      playing: ->
        $scope.playing = 1
        return
      ratechange: ->
        $scope.playing = video[0].playbackRate
        return
      timeupdate: ->
        if $scope.asset && isFinite($scope.asset.segment.l)
          o = Math.round(1000*video[0].currentTime)
          if $scope.editing == 'position' && $scope.asset == $scope.current.asset
            $scope.current.setPosition(ruler.position.o - o)
          else
            ruler.position.o = $scope.asset.segment.l + o
            if ruler.selection.uBounded && ruler.position.o >= ruler.selection.u
              video[0].pause()
              seekOffset(ruler.selection.l) if ruler.selection.lBounded
        return
      ended: ->
        $scope.playing = 0
        # look for something else to play?
        return

    for ev, fn of videoEvents
      videoEvents[ev] = $scope.$lift(fn)

    @deregisterVideo = (v) ->
      return unless video == v
      video = undefined
      v.off(videoEvents)
      return

    @registerVideo = (v) ->
      @deregisterVideo video if video
      video = v
      seekOffset(ruler.position.o)
      v.on(videoEvents)
      return

    ################################### Player display

    playerMinHeight = 200
    viewportMinHeight = 120
    playerHeight = parseInt(storage.get('player-height'), 10) || 400
    ### jshint ignore:start ###
    unless playerHeight >= playerMinHeight
      playerHeight = playerMinHeight
    ### jshint ignore:end ###

    playerImgHeightStyle = undefined
    playerVideoHeightStyle = undefined
    playerPDFHeightStyle = undefined
    updatePlayerHeight = () ->
      $timeout ->
        player = document.getElementById('player-scroll')
        return unless player
        viewer = document.getElementById('player-viewport')
        d = player.offsetTop + playerHeight - viewer.offsetTop - 10 # viewport padding+border
        if d < viewportMinHeight
          d = viewportMinHeight
        playerImgHeightStyle = styles.set('.player-viewport .asset-display img{max-height:'+d+'px}', playerImgHeightStyle)
        playerVideoHeightStyle = styles.set('.player-viewport .asset-display video,.player-drop{height:'+d+'px}', playerVideoHeightStyle)
        playerPDFHeightStyle = styles.set('.player-viewport .asset-display object,.player-drop{height:'+ (d - 10) + 'px}', playerPDFHeightStyle)
        return
      return
    setPlayerHeight = () ->
      storage.set('player-height', playerHeight)
      $scope.playerHeight = playerHeight
      updatePlayerHeight()
      return
    setPlayerHeight()

    $scope.updatePlayerHeight = updatePlayerHeight

    $scope.resizePlayer = (down, up) ->
      frame = document.getElementById('slot-player')
      frame.style.overflow = 'visible'
      player = document.getElementById('player-scroll')
      bar = down.currentTarget
      h = Math.max(player.offsetHeight + up.clientY - down.clientY, playerMinHeight)
      if up.type == 'mousemove'
        bar.style.top = h - player.offsetHeight + 'px'
        bar.style.borderTop = "2px solid #f26363"
      else
        frame.style.overflow = 'hidden'
        bar.style.top = '0px'
        bar.style.borderTop = "none"
        playerHeight = h
        setPlayerHeight()
      return

    ################################### Track management

    stayDirty = (global) ->
      if global || editing && $scope.current && $scope.form.edit && ($scope.current.asset || $scope.current.record) && ($scope.current.dirty = $scope.form.edit.$dirty)
        not confirm(constants.message('navigation.confirmation'))

    class TimeBar extends TimeSegment
      choose: ->
        return false if stayDirty()

        $scope.current = this
        $scope.asset = this?.asset if !this || this.type == 'asset'
        searchLocation($location.replace())
        delete target.asset
        delete target.record
        if $scope.form.edit
          if this?.dirty
            $scope.form.edit.$setDirty()
          else
            $scope.form.edit.$setPristine()

        blank.fillData() if blank && this == blank
        $scope.playing = 0
        finalizeSelection()
        updatePlayerHeight()
        true

      click: (event) ->
        return false if $scope.editing == 'position'
        if !this || $scope.current == this
          new TimePoint(event.clientX, 'x').seek()
        else
          @choose()
        return

      # Generic function that takes in a time, then will determine if it's
      # close enough to do a premiere-esque "snap" feature to the nearest
      # object.
      snapping: (pos) ->
        # Let's start with an empty array, which will contain all the times
        # to compare against.
        listOfAllPlacements = []

        # First, let's make a giant array of all the items we want to compare
        # times against.  Then let's extract all the times for the objects into
        # an even bigger array.
        ### jshint ignore:start ###
        for i in $scope.assets.concat(records, $scope.consents) when i isnt this
          # We don't want to have the item snap to itself.

          # We want to have all the times that are finite in our array to compare against
          listOfAllPlacements.push i.lt if i.lt.defined
          listOfAllPlacements.push i.ut if i.ut.defined
        ### jshint ignore:end ###

        # If there aren't any items in the timeline that we can snap to, let's just break
        # out and return the original time sent in.
        return pos unless listOfAllPlacements.length

        # We'll utilize the lodash `_.min` function to find the smallest value based on a
        # function we send in.  This function checks the distance (in pixels) from the
        # current items
        min = _.min listOfAllPlacements, (i) ->
          Math.abs(pos.x - i.x)

        # If the smallest value in the array was less than ten pixels away from an item,
        # send back that item's value.
        if Math.abs(pos.x - min.x) <= 10
          min
        else
          pos

    unchoose = TimeBar.prototype.choose.bind(undefined)
    $scope.click = TimeBar.prototype.click.bind(undefined)

    ################################### Selection (temporal) management

    $scope.setSelectionEnd = (u) ->
      pos = ruler.position.o
      if u == undefined
        ruler.selection = new TimeSegment(pos)
      else
        sel = ruler.selection
        sel = slot.segment if sel.empty
        ruler.selection =
          if u
            new TimeSegment(Math.min(sel.l, pos), pos)
          else
            new TimeSegment(pos, Math.max(sel.u, pos))
      finalizeSelection()
      return

    $scope.dragSelection = (down, up, c) ->
      return false if $scope.editing == 'position' || c && $scope.current != c

      startPos = down.position ?= new TimePoint(down.clientX, 'x')
      endPos = new TimePoint(up.clientX, 'x')
      endPos.clip()
      ruler.selection =
        if startPos.x < endPos.x
          new TimeSegment(startPos, endPos)
        else if startPos.x > endPos.x
          new TimeSegment(endPos, startPos)
        else if startPos.x == endPos.x
          new TimeSegment(startPos)
        else
          new TimeSegment(null)
      finalizeSelection() if up.type != 'mousemove'
      return

    $scope.zoom = (seg) ->
      if seg
        ruler.range = new Segment((if seg.lBounded then seg.l else fullRange.l), (if seg.uBounded then seg.u else fullRange.u))
        ruler.zoomed = true
      else
        ruler.range = fullRange
        ruler.zoomed = false
      resetRange()
      searchLocation($location.replace())
      return

    $scope.updateSelection = finalizeSelection = ->
      if editing
        return false if $scope.editing == 'position'
        $scope.editing = true
        $scope.current.updateExcerpt() if $scope.current?.excerpts
      for t in $scope.tags
        t.update()
      for c in $scope.comments
        c.update()
      return

    getSelection = ->
      if ruler.selection.empty
        new TimeSegment(if ruler.position.defined() then ruler.position)
      else
        ruler.selection

    ################################### Track implementations

    class Asset extends TimeBar
      constructor: (asset) ->
        @setAsset(asset)
        @excerpts = []
        return

      type: 'asset'

      reset: ->
        super()
        for e in @excerpts
          e.reset()
        return

      setAsset: (@asset) ->
        @fillData()
        if asset
          @init(asset.segment)
          @choose() if `asset.id == target.asset`
          $scope.asset = asset if $scope.current == this
          @updateExcerpt()
        else
          @init(undefined)
        return

      fillData: ->
        @data =
          if @asset
            name: @asset.name
            classification: (@asset.classification || 0)+''
          else
            classification: '0'
        return

      Object.defineProperty @prototype, 'id',
        get: -> @asset?.id

      Object.defineProperty @prototype, 'name',
        get: ->
          return constants.message('asset.add') unless @file || @asset
          @asset?.name ? @data.name ? @file?.file.name ? constants.message('file')

      removed: ->
        return if @asset || @file
        unchoose() if @ == $scope.current
        blank = undefined if @ == blank
        $scope.assets.remove(@)
        return

      remove: ->
        messages.clear(this)
        return if @pending # sorry
        return unless confirm constants.message 'asset.remove.confirm'
        if @file
          @file.cancel()
          delete @file
          return @removed()
        return @removed() unless @asset
        @asset.remove().then (asset) =>
            uploads.removedAsset = asset
            messages.add
              type: 'green'
              body: constants.message('asset.remove.success', @name)
              owner: this
            delete @asset
            @removed()
          , (res) =>
            messages.addError
              type: 'red'
              body: constants.message('asset.remove.error', @name)
              report: res
              owner: this
        return

      save: ->
        return if @pending # sorry
        $scope.form.edit.$setSubmitted()
        @pending = 1
        messages.clear(this)
        (if @file
          @data.upload = @file.uniqueIdentifier
          if @asset then @asset.replace(@data) else slot.createAsset(@data)
        else
          @asset.save(@data)
        ).then (asset) =>
            delete @pending

            $scope.form.edit.$setUnsubmitted()
            first = !@asset
            @setAsset(asset)

            messages.add
              type: 'green'
              body: constants.message('asset.' + (if @file then (if first then 'upload' else 'replace') else 'update') + '.success', @name) +
                (if @file && asset.format.transcodable then ' ' + constants.message('asset.upload.transcoding') else '')
              owner: this

            if @file
              asset.creation ?= {date: Date.now(), name: @file.file.name}
              @file.cancel()
              delete @file
              delete @progress
            delete @dirty
            $scope.form.edit.$setPristine() if this == $scope.current
            updateRange()
            Asset.sort()
            return
          , (res) =>
            delete @pending
            $scope.form.edit.$setUnsubmitted()
            messages.addError
              type: 'red'
              body: constants.message('asset.update.error', @name)
              report: res
              owner: this
            if @file
              @file.cancel()
              delete @file
              delete @progress
              delete @data.upload
            return
        return

      upload: (file) ->
        blank = undefined if this == blank
        messages.clear(this)
        return if @file
        file.pause()
        @file = file
        @progress = 0
        file.store = this

        router.http(router.controllers.AssetApi.uploadStart, slot.volume.id,
            filename: file.name
            size: file.size
          ).then (res) =>
            file.uniqueIdentifier = res.data
            file.resume()
            ### jshint ignore:start ###
            @data.name ||= file.file.name
            ### jshint ignore:end ###
            return
          , (res) =>
            messages.addError
              type: 'red'
              body: constants.message('asset.upload.rejected', {sce:$sce.HTML}, @name)
              report: res
              owner: this
            file.cancel()
            delete @file
            delete @progress
            @removed()
            false
        return

      rePosition: () ->
        $scope.editing = 'position'
        return

      updatePosition: () ->
        @u = @l + (@asset.duration || 0)
        return

      setPosition: (p) ->
        if p instanceof TimePoint
          @lt = p
        else
          @l = p
        @updatePosition()
        $scope.form.position.$setDirty()
        return

      finishPosition: () ->
        $scope.form.position.$setPristine()
        $scope.editing = true
        @init(@asset.segment)
        return

      savePosition: () ->
        messages.clear(this)
        shift = @asset?.segment.l
        @asset.save({container:slot.id, position:Math.floor(@l)}).then (asset) =>
            @asset = asset
            shift -= @asset.segment.l
            if isFinite(shift) && shift
              for e in @excerpts
                e.l -= shift
                e.u -= shift
            updateRange()
            Asset.sort()
            @finishPosition()
            @updateExcerpt()
          , (res) =>
            @finishPosition()
            messages.addError
              type: 'red'
              body: constants.message('asset.update.error', @name)
              report: res
              owner: this
            return

      dragMove: (down, up) ->
        offset = down.offset ?= new TimePoint(down.clientX, 'x').minus(@lt)
        pos = new TimePoint(up.clientX, 'x').minus(offset)
        return unless pos.defined()
        pos = @snapping(pos)
        @setPosition(pos)
        if up.type != 'mousemove'
          $scope.updatePosition()
        return

      updateExcerpt: () ->
        @excerpt = undefined
        return unless @asset && @excerpts
        seg = if @full then this else getSelection()
        return if !@asset || !seg || !@overlaps(seg)
        e = @excerpts.find((e) -> seg.overlaps(e))
        @excerpt =
          if !e
            target: @asset.inSegment(seg)
            on: false
            release: ''
          else if e.equals(seg)
            current: e
            target: e.excerpt
            on: true
            release: e.excerpt.excerpt+''
          else
            null
        return

      editExcerpt: () ->
        @updateExcerpt() # should be unnecessary
        $scope.editing = 'excerpt'
        return

      excerptOptions: () ->
        l = {}
        r = @asset.release || constants.release.PRIVATE
        l[0] = constants.message('release.DEFAULT.select') + ' (' + constants.message('release.' + constants.release[r] + '.title') + ')'
        for c, i in constants.release when i > r
          l[i] = constants.message('release.' + c + '.title') + ': ' + constants.message('release.' + c + '.select')
        l[@excerpt.release] = constants.message('release.prompt') unless @excerpt.release of l
        l

      saveExcerpt: (value) ->
        $scope.editing = true
        if value == undefined || value == ''
          return
        messages.clear(this)
        if !@asset.classification && value > (slot.release || constants.release.PRIVATE) && !confirm(constants.message('release.excerpt.warning'))
          return
        @excerpt.target.setExcerpt(value)
          .then (excerpt) =>
              @excerpts.remove(@excerpt.current)
              if 'excerpt' of excerpt
                @excerpts.push(new Excerpt(excerpt))
              @updateExcerpt()
            , (res) =>
              messages.addError
                type: 'red'
                body: constants.message('asset.update.error', @name)
                report: res
                owner: this

      canRestore: () ->
        uploads.removedAsset? if editing && this == blank && uploads.removedAsset?.volume.id == slot.volume.id

      restore: () ->
        messages.clear(this)
        uploads.removedAsset.link(slot).then (a) =>
            uploads.removedAsset = undefined
            @setAsset(a)
            blank = undefined if this == blank
            return
          , (res) ->
            messages.addError
              type: 'red'
              body: constants.message('asset.update.error', '[removed file]')
              report: res
              owner: this
            return
        return

      @sort = ->
        return unless $scope.assets
        $scope.assets.sort (a, b) ->
          if a.asset && b.asset
            isFinite(b.asset.segment.l) - isFinite(a.asset.segment.l) ||
              a.asset.segment.l - b.asset.segment.l ||
              a.asset.segment.u - b.asset.segment.u ||
              a.id - b.id
          else
            !a.asset - !b.asset || !a.file - !b.file
        return

    class Excerpt extends TimeBar
      constructor: (e) ->
        super(e.segment)
        @excerpt = e
        return

      @fill = ->
        assets = {}
        for t in $scope.assets when t.asset
          assets[t.asset.id] = t
        for e in slot.excerpts
          assets[e.id]?.excerpts.push(new Excerpt(e))
        return

    $scope.addBlank = ->
      unless blank
        $scope.assets.push(blank = new Asset())
      blank.choose()
      blank

    $scope.fileAdded = (file) ->
      $flow = file.flowObj
      (!$scope.current?.file && $scope.current || $scope.addBlank()).upload(file) if editing
      return

    $scope.fileSuccess = uploads.fileSuccess
    $scope.fileProgress = uploads.fileProgress

    class Record extends TimeBar
      constructor: (r) ->
        @rec = r
        @record = r.record || slot.volume.records[r.id]
        for f in ['age'] when f of r
          @[f] = r[f]
        super(r.segment)
        if editing
          @fillData()
        return

      type: 'record'

      fillData: ->
        @data =
          measures: angular.extend({}, @record.measures)
          add: ''
        @sortMetrics() if editing
        return

      sortMetrics: ->
        keys = _.keys @data.measures
        @sortedMetrics = keys.sort (a,b) -> a - b
        return

      Object.defineProperty @prototype, 'id',
        get: -> @rec.id

      remove: ->
        messages.clear(this)
        slot.removeRecord(@record, this).then (r) =>
            return unless r
            records.remove(this)
            unchoose() if $scope.current == this
            Record.place()
            return
          , (res) ->
            messages.addError
              type: 'red'
              body: 'Unable to remove'
              report: res
              owner: this
            return

      ### jshint ignore:start #### fixed in jshint 2.5.7
      metrics: ->
        ident = constants.category[@record.category]?.ident || [constants.metricName.ID.id]
        (constants.metric[m] for m of @record.measures when !(+m in ident)).sort(byId)

      addMetric = {id:'',name:'Add new value...'}
      addOptions: ->
        metrics = (metric for m, metric of constants.metric when !(m of @data.measures)).sort(byId)
        metrics.unshift addMetric
        metrics
      ### jshint ignore:end ###

      add: ->
        @data.measures[@data.add] = '' if @data.add
        @data.add = ''
        @sortMetrics()
        return

      save: ->
        messages.clear(this)
        @record.save({measures:@data.measures}).then () =>
            @fillData()
            delete @dirty
            if this == $scope.current
              $scope.form.edit.$setPristine()
              $scope.form.measures.$setPristine()
            return
          , (res) =>
            messages.addError
              type: 'red'
              body: 'Error saving record'
              report: res
              owner: this
            return

      rePosition: () ->
        $scope.editing = 'position'
        return

      updatePosition: (u) ->
        $scope.form.position[if u then 'position-lower' else 'position-upper'].$validate()
        return

      finishPosition: () ->
        $scope.editing = true
        @init(@rec.segment)
        $scope.form.position.$setPristine()
        return

      savePosition: () ->
        messages.clear(this)
        slot.moveRecord(@record, @rec.segment, this).then (r) =>
            if r && @empty
              records.remove(this)
              unchoose() if this == $scope.current
            @finishPosition()
            if r
              Record.place()
              updateRange()
            return
          , (res) =>
            messages.addError
              type: 'red'
              body: 'Error saving record'
              report: res
              owner: this
            return

      drag: (event, which) ->
        x = new TimePoint(event.clientX, 'x')
        this[if which then 'ut' else 'lt'] =
          if which && x.p > 1
            new TimePoint(Infinity)
          else if !which && x.p < 0
            new TimePoint(-Infinity)
          else
            @snapping(x)
        if @empty
          if which
            @lt = @ut
          else
            @ut = @lt
        if event.type != 'mousemove'
          $scope.form.position.$setDirty()
        return

      @place = () ->
        t = []
        overlaps = (rr) -> rr.record.id != r.record.id && r.overlaps(rr)
        for r in records
          for o, i in t
            break unless o[0].record.category != r.record.category || o.some(overlaps)
          t[i] = [] unless i of t
          t[i].push(r)
          r.choose() if `r.id == target.record`
        for r in t
          r.sort byPosition
        $scope.records = t
        return

    $scope.positionBackgroundStyle = (l, i) ->
      if l[i].lt.p > 1 || l[i].ut.p < 0
        visibility: "hidden"
      else
        new TimeSegment(l[i].l, if i+1 of l then l[i+1].l else Infinity).style()

    $scope.setCategory = (c) ->
      if c?
        rs = {}
        for ri, r of $scope.addRecord.records when `(r.category || 0) == c`
          rs[ri] = r.displayName
        $scope.addRecord.options = rs
      else
        $scope.addRecord.options = undefined
      return

    $scope.addRecord = (r) ->
      seg = getSelection()
      if r == undefined
        $scope.editing = 'record'
        rs = {}
        for ri, r of slot.volume.records
          rs[ri] = r
        for sr in records
          if sr.record.id of rs && sr.overlaps(seg)
            delete rs[sr.record.id]
        $scope.addRecord.records = rs
        rc = {}
        for ri, r of rs
          rc[r.category || 0] = null
        $scope.addRecord.categories = rc
        $scope.addRecord.select = null
        $scope.setCategory($scope.addRecord.category)
      else
        $scope.addRecord.records = undefined
        unless r?
          $scope.editing = true
          return
        slot.addRecord(slot.volume.records[r], seg).then (r) ->
            $scope.editing = true
            r = new Record(r)
            records.push(r)
            Record.place()
            r.choose()
            return
          , (res) ->
            $scope.editing = true
            messages.addError
              body: 'Error adding record'
              report: res
              owner: $scope
            return
      return

    class Consent extends TimeBar
      constructor: (c) ->
        if typeof c == 'object'
          @release = c.release
          super(c.segment)
        else
          @release = c
          super(undefined)
        return

      type: 'consent'

      classes: ->
        cn = constants.release[@release]
        cls = [cn, 'hint-release-' + cn]
        cls.push('slot-release-select') if $scope.current == this
        cls

    class TagName extends TimeBar
      constructor: (name) ->
        @id = name
        super()
        return

      save: (vote) ->
        seg = getSelection()
        tag = this
        slot.setTag(@id, vote, editing, seg).then (data) ->
            unless tag instanceof Tag
              tag = $scope.tags.find (t) -> t.id == data.id
              unless tag
                tag = new Tag(data)
                tag.toggle(true)
                $scope.tags.push(tag)
            tag.fillData(data)
            if (if editing then tag.keyword?.length else tag.coverage?.length)
              tag.update()
            else
              $scope.tags.remove(tag)
            tooltips.clear() # hack for broken tooltips
            return
          , (res) =>
            messages.addError
              body: constants.message('tags.vote.error', {sce: $sce.HTML}, @id)
              report: res
              owner: $scope
            return

    $scope.vote = (name, vote) ->
      new TagName(name).save(vote)

    tagToggle = storage.get('tag-toggle')?.split("\n") ? []

    class Tag extends TagName
      constructor: (t) ->
        super(t.id)
        @active = t.id in tagToggle
        @fillData(t)
        return

      type: 'tag'

      reset: ->
        for f in (if editing then ['keyword'] else ['coverage','vote','keyword'])
          for t in this[f]
            t.reset()
        return

      fillData: (t) ->
        @weight = t.weight
        for f in (if editing then ['keyword'] else ['coverage','vote','keyword'])
          this[f] = []
          if t[f]
            for s in t[f]
              this[f].push(new TimeSegment(s))
        return

      toggle: (act) ->
        if @active = act ? !@active
          tagToggle.push(@id)
        else
          tagToggle.remove(@id)
        storage.set('tag-toggle', tagToggle.join("\n"))

      update: ->
        state = false
        sel = getSelection()
        for s in (if editing then @keyword else @vote)
          if sel.contains(s)
            state = true
          else if sel.overlaps(s)
            state = undefined
            break
        @state = state

    class Comment extends TimeBar
      constructor: (c) ->
        @comment = c
        super(c.segment)

      type: 'comment'

      update: ->
        @classes = []
        if @comment.parents
          @classes.push('depth-' + Math.min(@comment.parents.length, 5))
        unless ruler.selection.empty || ruler.selection.overlaps(this) || this == $scope.replyTo
          @classes.push('notselected')

      setReply: (event) ->
        $scope.form.comment?.text = ''
        $scope.form.comment?.reply = ''
        $scope.commentReply = if event
          this.select(event)
          this
        finalizeSelection()

    $scope.addComment = (message, replyTo) ->
      slot.postComment {text:message}, getSelection(), replyTo?.comment.id
      .then () ->
          slot.getSlot(slot.segment, ['comments']).then((res) ->
              $scope.form.comment.text = $scope.form.reply = ''
              $scope.comments = (new Comment(comment) for comment in res.comments)
              comment.update() for comment in $scope.comments
              return
            , (res) ->
                messages.addError
                  body: constants.message('comments.update.error')
                  report: res
            )
        , (e) ->
          messages.addError
            body: constants.message('comments.add.error')
            report: e

    ################################### Initialization

    ### jshint ignore:start #### fixed in jshint 2.5.7
    $scope.tags = (new Tag(tag) for tagId, tag of slot.tags when (if editing then tag.keyword?.length else tag.coverage?.length))
    $scope.comments = (new Comment(comment) for comment in slot.comments)
    $scope.assets = (new Asset(asset) for assetId, asset of slot.assets)
    ### jshint ignore:end ###
    Asset.sort()
    Excerpt.fill()

    records = slot.records.map((r) -> new Record(r))

    $scope.consents =
      if Array.isArray(consents = slot.releases)
        _.map consents, (c) -> new Consent(c)
      else if (consents)
        [new Consent(consents)]
      else
        []

    $scope.playing = 0
    Record.place()
    updateRange()
    finalizeSelection()

    if editing
      done = $scope.$on '$locationChangeStart', (event, url) ->
        return if url.includes(slot.editRoute())
        if $flow
          uploading = $flow.isUploading()
          $flow.pause()
        if stayDirty(uploading)
          $flow.resume() if uploading
          display.cancelRouteChange(event)
        else
          done()
        return

    $scope.$on '$destroy', ->
      $flow?.cancel()
      return

    return
])
