'use strict'

app.directive 'spreadsheet', [
  'constantService', 'displayService', 'messageService', 'tooltipService', 'styleService', '$compile', '$templateCache', '$timeout', '$document', '$location',
  (constants, display, messages, tooltips, styles, $compile, $templateCache, $timeout, $document, $location) ->
    maybeInt = (s) ->
      if isNaN(i = parseInt(s, 10)) then s else i
    byDefault = (a,b) -> +(a > b) || +(a == b) - 1
    byNumber = (a,b) -> a-b
    byType = (a,b) ->
      ta = typeof a
      tb = typeof b
      if ta != tb
        a = ta
        b = tb
      byDefault(a,b)
    byMagic = (a,b) ->
      if isNaN(d = a-b) then byType(a,b) else d
    bySortId = (a,b) ->
      (a.sort || a.id)-(b.sort || b.id)
    parseIntish = (c) ->
      if isNaN(i = parseInt(c, 10)) then c else i

    stripPrefix = (s, prefix) ->
      if s.startsWith(prefix) then s.substr(prefix.length)

    # autovivification
    arr = (a, f) ->
      if f of a then a[f] else a[f] = []
    obj = (a, f) ->
      if f of a then a[f] else a[f] = {}
    inc = (a, f) ->
      if f of a then a[f]++ else
        a[f] = 1
        0

    pseudoMetric =
      top: # slot
        id: 'top'
        name: 'type'
        type: 'top'
        sort: -10000
        options:
          false: 'session'
          true: 'materials'
      name: # slot, asset
        id: 'name'
        name: 'name'
        type: 'text'
        sort: -9000
      date: # slot
        id: 'date'
        name: 'test date'
        type: 'date'
        description: 'Date on which this session was acquired'
        sort: -8000
      release: # slot
        id: 'release'
        name: 'release'
        type: 'release'
        description: 'Level of data release to which depicted participant(s) consented'
        sort: -7000
      age: # record
        id: 'age'
        name: 'age'
        type: 'number'
        release: constants.release.EXCERPTS
        sort: constants.metricName.birthdate.id + 0.5
        description: 'Time between birthdate and test date'
        readonly: true
      summary: # slot
        id: 'summary'
        name: 'summary'
        type: 'text'
        sort: 10000
        readonly: true
    constants.deepFreeze(pseudoMetric)
    getMetric = (m) ->
      pseudoMetric[m] || constants.metric[m]

    {
    restrict: 'E'
    scope: true
    templateUrl: 'volume/spreadsheet.html'
    controller: [
      '$scope', '$element', '$attrs',
      ($scope, $element, $attrs) ->
        volume = $scope.volume

        Editing = $scope.editing = $attrs.edit != undefined
        Top = $scope.top = $attrs.top != undefined
        Top ||= undefined if Editing
        ID = $scope.id = $attrs.id ? if Top then 'sst' else 'ss'
        Limit = $attrs.limit
        Key = undefined

        pseudoCategory =
          slot:
            id: 'slot'
            name: if Top? then pseudoMetric.top.options[Top] else 'folder'
            not: 'No ' + (if Top then 'materials' else if Top == false then 'sessions' else 'folders')
            metrics: if Top then ['name'] else if Top == false then ['name', 'date', 'release'] else ['top', 'name', 'date', 'release']
            fixed: true
          asset:
            id: 'asset'
            name: 'file'
            not: 'No files'
            metrics: ['name']
            description: 'Files included in this folder'
            fixed: true
        constants.deepFreeze(pseudoCategory)
        getCategory = (c) ->
          pseudoCategory[c] || constants.category[c]

        ###
        # We use the following types of data structures:
        #   Row = index of slot in slots and rows (i)
        #   Slot_id = Database id of container
        #   Segment = standard time range (see type service)
        #   Record_id = Database id of record
        #   Category_id = Database id of record category (c)
        #   Count = index of record within category for slot (n)
        #   Metric_id = Database id of metric, or "id" for Record_id, or "age" (m)
        ###

        TBody = $element[0].getElementsByTagName("tbody")[0]
        TFoot = $element[0].getElementsByTagName("tfoot")[0]

        Groups = []     # [] Array over categories :: {category: Category, metrics[]: Array of Metric}
        Cols = []       # [] Array over metrics :: {category: Category, metric: Metric} (flattened version of Groups)
        Rows = []       # [i] :: Row (-1 => foot)
        Cats = {}       # Set of used categories
        Order = []      # Permutation Array of Row in display order
        Expanded = undefined # Info


        class Row
          # Represents everything about a row.  Properties:
          #   i: Row index
          #   tr: DOM Element tr
          #   [Category_id]: (array of) metric data for category

          constructor: (i) ->
            @i = i ? Rows.length
            Rows[@i] = this
            return

          add: (c, d) ->
            Cats[c] = true
            if v = this[c]
              if Array.isArray(v)
                n = v.push(d) - 1
              else
                this[c] = [v, d]
                n = 1
            else
              this[c] = d
              n = 0
            n

          list: (c) ->
            if v = this[c]
              if Array.isArray(v)
                v
              else
                [v]
            else
              []

          count: (c) ->
            if v = this[c]
              if Array.isArray(v)
                v.length
              else
                1
            else
              0

          get: (c, n) ->
            if v = this[c]
              if Array.isArray(v)
                v[n || 0]
              else if !n
                v

          set: (c, n, d) ->
            n ||= 0
            if n || Array.isArray(this[c])
              if d?
                this[c].splice(n, 1, d)
              else
                this[c].splice(n, 1)
            else
              this[c] = d
            return

          Object.defineProperty @prototype, 'key',
            get: -> @get(Key.id)


        class Info
          # Represents everything we know about a specific cell.  Properties:
          #   cell: target TD element
          #   id: cell.id
          #   i: Row
          #   n: Count (index of count), optional [0]
          #   m: index into Cols
          #   cols: Groups element
          #   col: Cols element
          #   category: Category
          #   c: Category_id
          #   count: Count[i][c]
          #   metric: Metric
          #   row: Rows[i]
          #   slot: Container
          #   d: Data
          #   record: Record
          #   asset: Asset
          #   v: Data value

          constructor: (x) ->
            if typeof x == 'number'
              @i = x
            else if x
              @cell = x
            @parseId()
            return

          parseId: (i) ->
            return unless (if i? then @id = i else i = @id) and (i = stripPrefix(i, ID+'-'))
            s = i.split '_'
            switch s[0]
              when 'add', 'more'
                @t = s[0]
                @i = parseInt(s[1], 10)
                @c = parseIntish(s[2])
              when 'metric'
                @t = s[0]
                @m = parseInt(s[1], 10)
              when 'category'
                @t = s[0]
                @c = parseIntish(s[1])
              else
                @i = parseInt(s[0], 10)
                @m = parseInt(s[1], 10)
                if 2 of s
                  @n = parseInt(s[2], 10)
            true

          properties =
            n: ->
              0
            id: ->
              @cell?.id
            cols: ->
              c = @c
              @cols = Groups.find (col) -> `col.category.id == c`
            col: ->
              Cols[@m]
            category: ->
              if (c = @col)?
                c.category
              else if this.hasOwnProperty('c')
                @category = getCategory(@c)
            c: ->
              @category?.id
            metric: ->
              @col?.metric
            row: ->
              Rows[@i]
            count: ->
              @row.count(@c)
            tr: ->
              @row.tr
            d: ->
              @row.get(@c, @n)
            p: ->
              cls = 'ss-'
              if typeof (c = @c) != 'number'
                cls += c.charAt(0)
              cls
            slot: ->
              @row.get('slot', if @c == 'slot' then @n)?.slot
            record: ->
              @d?.record
            asset: ->
              @d?.asset
            v: ->
              if d = @d
                d[@metric.id]

          property = (v, f) ->
            get: f
            set: (x) ->
              Object.defineProperty @, v,
                value: x
                writable: true
                configureable: true
                enumerable: true
              return

          for v, f of properties
            properties[v] = property(v, f)

          Object.defineProperties @prototype,
            properties

        parseId = (el) ->
          return unless el.tagName == 'TD'
          info = new Info(el)
          info if info.c?

        ################################# Populate data structures

        # Fill Cols and Groups from volume metrics
        populateCols = (slot) ->
          Cols = []
          if slot
            slot = undefined
            cats = Object.keys(volume.metrics)
            unless Editing
              cats = cats.filter((i) -> Cats[i])
            cats = cats.sort(byNumber).map((i) -> constants.category[i])
            if Top
              cats.unshift(pseudoCategory.asset)
            else
              cats.push(pseudoCategory.asset)
            cats.unshift(pseudoCategory.slot)
          else
            slot = [pseudoMetric.summary]
            cats = [Key, pseudoCategory.slot]

          $scope.groups = Groups = cats.map (category) ->
            metrics = (category.metrics || volume.metrics[category.id]).map(getMetric)
            if !Editing && constants.metricName.birthdate in metrics
              (slot || metrics).push(pseudoMetric.age)
            if slot && category.id == 'slot'
              metrics.push.apply metrics, slot
            metrics.sort(bySortId)
            si = Cols.length
            Cols.push.apply Cols, _.map metrics, (m) ->
              category: category
              metric: m
            l = metrics.length
            Cols[si].first = Cols[si+l-1].last = l
            {
              category: category
              metrics: metrics
              start: si
            }
          $scope.cols = Cols
          $scope.views = (g.category for g in Groups when g.category.id != 'asset')
          return

        populateSlotData = (s) ->
          d =
            slot: s
            id: s.id
            top: !!s.top
            name: s.name
            date: s.date
            release: s.release+''
          d

        populateRecordData = (r) ->
          d = Object.create(r.measures)
          d.record = r
          d.id = r.id
          d

        populateAssetData = (a) ->
          d =
            asset: a
            id: a.id
            name: a.name
          d

        # Fill all Data values for Row i
        populateSlot = (slot) ->
          row = new Row()
          row.add('slot', populateSlotData(slot))
          row.top = true if slot.top && slot.id == volume.top.id

          for rr in slot.records
            record = rr.record
            # temporary workaround for half-built volume inclusions:
            continue unless record

            d = populateRecordData(record)
            if 'age' of rr
              d.age = rr.age
            row.add(record.category, d)

          for assetId, asset of slot.assets
            row.add('asset', populateAssetData(asset))

          row

        populateSlots = () ->
          for ci, slot of volume.containers when Top != !slot.top
            populateSlot(slot)

        populateRecord = (record) ->
          row = new Row()
          row.add(record.category, populateRecordData(record))
          row

        populateRecords = () ->
          records = {}
          for r, record of volume.records when record.category == Key.id
            records[r] = populateRecord(record)

          nor = undefined
          for s, slot of volume.containers when Top != !slot.top && slot.id != volume.top.id
            recs = slot.records
            any = false
            for rr in recs when (row = records[rr.id])
              d = populateSlotData(slot)
              if 'age' of rr
                d.age = rr.age
              d.summary = (rrr.record.displayName for rrr in recs when rrr.id != rr.id).join(', ')
              row.add('slot', d)
              any = true
            unless any
              nor ||= new Row()
              d = populateSlotData(slot)
              d.summary = (rrr.record.displayName for rrr in recs).join(', ')
              nor.add('slot', d)
              
        # Call all populate functions
        populate = ->
          foot = Rows[-1]
          Rows = []
          if bySlot = Key == pseudoCategory.slot
            populateSlots()
          else
            populateRecords()
          if Order.length != Rows.length
            Order = if Rows.length then [0..Rows.length-1] else []
          $(TBody).empty()
          Expanded = undefined
          Rows[-1] = foot if foot
          populateCols(bySlot)
          generate()
          tooltips.clear()
          return

        ################################# Generate HTML

        # Add or replace the text contents of cell c for measure/type m with value v
        generateText = (info) ->
          $(cell = info.cell).empty()
          cell.className = ''
          v = info.v
          slot = info.slot
          if info.col.first && info.d
            if info.c == 'asset'
              a = cell.appendChild(document.createElement('a'))
              icon = a.appendChild(document.createElement('img'))
              asset = info.asset
              icon.src = asset.icon
              icon.className = "format hint-format-" + asset.format.extension
              t = {asset:asset.id}
              a.setAttribute('href', if Editing then slot.editRoute(t) else slot.route(t))
              icon = cell.appendChild(document.createElement('span'))
              icon.className = 'icon release ' + constants.release[asset.release] + ' hint-release-' + constants.release[asset.release]
            else
              if Editing && Key.id == info.c && info.row.top
                cell.classList.add('top-level-materials')
                delspace = cell.appendChild(document.createElement('span'))
                delspace.className = 'icon'
              if Editing && Key.id == info.c && !info.row.top
                cell.classList.add('folder-type')
                del = cell.appendChild(document.createElement('a'))
                del.className = 'trash icon'
                i = new Info(info.i)
                i.c = info.c
                i.cell = cell
                $(del).on 'click', $scope.$lift(clickRemove)
              if info.c == 'slot'
                a = cell.appendChild(document.createElement('a'))
                a.className = "session icon hint-action-slot"
                a.setAttribute('href', if Editing then slot.editRoute() else slot.route())
          switch info.metric.id
            when 'name'
              if info.row.top && info.c == 'slot'
                cell.classList.add('top-level-materials')
                v ?= constants.message('materials.top')
              else
                v ?= ''
            when 'release'
              cn = constants.release[v]
              cell.className = cn + ' release icon hint-release-' + cn
              cell.classList.add('null') if slot?.top
              v = ''
            when 'date'
              cell.classList.add('null') if slot?.top
            when 'age'
              v = display.formatAge(v)
            when 'top'
              v = info.metric.options[v]
            else
              if info.metric.type == 'void' && info.d
                cell.className = 'icon ' + if Editing && Key.id != info.c then 'trash' else 'bullet'
                v = ''
          if info.metric.long
            cell.classList.add('long')
          if v?
            cell.classList.remove('blank')
          else
            cell.classList.add('blank')
            v = info.metric.assumed || ''
          cell.appendChild(document.createTextNode(v))
          cell.id = info.id
          if info.d
            cell.classList.add(cls = info.p + info.d.id)
            cell.classList.add(cls + '_' + info.metric.id)
          return

        generateAdd = (info, td) ->
          info.m = info.cols.start
          width = info.width || info.cols.metrics.length
          if typeof info.metric.id == 'number' && info.metric.type != 'void'
            info.id = ID+'-'+info.i+'_'+info.cols.start+(if info.hasOwnProperty('n') then '_'+info.n else '')
            info.cell = info.tr.appendChild(document.createElement('td'))
            generateText(info)
            info.tr.insertBefore(info.cell, td)
            if width > 1
              td.setAttribute("colspan", width-1)
              td.appendChild(document.createTextNode("\u2190 add " + info.category.name))
            else
              info.tr.removeChild(td)
          else
            td.setAttribute("colspan", width)
            td.classList.add('add')
            td.id = ID + '-add_' + info.i + '_' + info.c
            td.appendChild(document.createTextNode("add " + info.category.name))

        generateMultiple = (info) ->
          t = info.count
          return if (if info.hasOwnProperty('n') then info.n < t else t == 1)
          td = info.tr.appendChild(document.createElement('td'))
          width = info.cols.metrics.length
          td.setAttribute("colspan", width)
          if info.hasOwnProperty('n') || t <= 1
            td.className = 'null'
            if !info.n || info.n == t
              if Editing && info.c != 'slot' && info.c != Key.id
                generateAdd(info, td)
              else if !info.n
                td.appendChild(document.createTextNode(info.category.not))
          else
            td.appendChild(document.createTextNode(t + " " + info.category.name + "s"))
            td.className = 'more'
            td.id = ID + '-more_' + info.i + '_' + info.c
          td

        # Add all the measure tds to row i for count n, record r
        generateRecord = (info) ->
          return unless l = info.cols.metrics.length # impossible?
          if td = generateMultiple(info)
            unless info.hasOwnProperty('n')
              cls = info.p
              for n in [0..info.count-1] by 1
                td.classList.add(cls + info.row.get(info.c, n).id)
            return
          pre = ID + '-' + info.i + '_'
          post = if info.hasOwnProperty('n') then '_' + info.n else ''
          for mi in [0..l-1] by 1
            info.m = info.cols.start+mi
            info.id = pre + (info.cols.start+mi) + post
            info.cell = info.tr.appendChild(document.createElement('td'))
            generateText(info)
          return

        # Fill out Rows[i].
        generateRow = (i) ->
          info = new Info(i)
          row = info.row
          if row.tr
            $(row.tr).empty()
            row.tr
          else
            row.tr = document.createElement('tr')
          tr = row.tr
          tr.id = ID + '_' + i
          tr.data = i
          if Editing && info.row.top
            tr.className = 'top'

          for col in Groups
            info.category = (info.cols = col).category
            generateRecord(info)
          return

        generateFoot = ->
          if row = Rows[-1]
            $(Rows[-1].tr).empty()
          else
            row = new Row(-1)
            row.tr = TFoot.appendChild(document.createElement('tr'))
            row.tr.id = ID + '_add'
          info = new Info(row.i)
          info.cols = Groups[0]
          info.category = Key
          td = info.tr.appendChild(document.createElement('td'))
          td.setAttribute("colspan", info.cols.metrics.length)
          td.className = 'null'
          info.width = Cols.length
          generateAdd(info, td)

        # Update all age displays.
        $scope.$on 'displayService-toggleAge', ->
          info = new Info()
          for m, mi in Cols
            continue unless m.metric.id == 'age'
            info.m = mi
            pre = ID + '-'
            mid = '_' + mi
            for i, n in Order
              info.i = i
              if info.v
                info.cell = document.getElementById(pre + i + mid)
                generateText(info) if info.cell
            if Expanded?.c == info.c && Expanded.count > 1
              info.i = Expanded.i
              premid = pre + info.i + mid + '_'
              for n in [0..Expanded.count-1] by 1
                info.n = n
                if info.v
                  info.cell = document.getElementById(premid + n)
                  generateText(info) if info.cell

        # Generate all rows.
        generate = ->
          for i, n in Order
            generateRow(i)
          fill()
          if Editing
            generateFoot()
          return

        ################################# Place DOM elements

        # Place all rows into spreadsheet.
        fill = ->
          collapse()
          delete $scope.more
          for i, n in Order
            if n >= Limit
              $scope.more = Order.length
              TBody.removeChild(Rows[i].tr) if Rows[i].tr.parentNode
            else
              TBody.appendChild(Rows[i].tr)
          return

        # Populate order based on compare function applied to values.
        sort = (get) ->
          idx = new Array(Order.length)
          for o, i in Order
            idx[o] = i
          Order.sort (i, j) ->
            byMagic(get(i), get(j)) || idx[i] - idx[j]
          return

        currentSort = undefined
        currentSortDirection = false

        # Sort by column
        sortBy = (col) ->
          if currentSort == col
            currentSortDirection = !currentSortDirection
            Order.reverse()
          else
            c = col.category.id
            m = col.metric.id
            sort (i) ->
              r = Rows[i]
              if r.top
                null
              else
                r.get(c)?[m]
            currentSort = col
            currentSortDirection = false
          fill()
          return

        $scope.colClasses = (col) ->
          cls = []
          if typeof col == 'object'
            cls.push 'first' if col.first
            cls.push 'last' if col.last
          cls.push 'sort'
          if currentSort == col
            cls.push 'intransitive sort-'+(if currentSortDirection then 'desc' else 'asc')
          else
            cls.push 'intransitive sortable'
          cls

        ################################# Backend saving

        setFocus = undefined

        saveRun = (cell, run) ->
          messages.clear(cell)
          cell.classList.remove('error')
          cell.classList.add('saving')
          run.then (res) ->
              cell.classList.remove('saving')
              $scope.form.$setPristine()
              res
            , (res) ->
              cell.classList.remove('saving')
              cell.classList.add('error')
              messages.addError
                body: 'Error saving data' # FIXME
                report: res
                owner: cell
              return

        addRow = (row) ->
          Order.push(row.i)
          generateRow(row.i)
          TBody.appendChild(row.tr)
          row

        createSlot = (info) ->
          saveRun info.cell, volume.createContainer({top:Top}).then (slot) ->
            arr(slot, 'records')
            addRow(populateSlot(slot))

        createRecord = (info) ->
          saveRun info.cell, volume.createRecord(info.c || undefined).then (record) ->
            addRow(populateRecord(record))

        createNew = (info) ->
          if info.c == 'slot'
            createSlot(info)
          else if typeof info.c == 'number'
            createRecord(info)

        removeRow = (i) ->
          unedit(false)
          Order.remove(i)
          Order = Order.map (j) -> j - (j > i)
          populate()
          return

        removeSlot = (info) ->
          # assuming we have a container
          saveRun info.cell, info.slot.remove().then (done) ->
            unless done
              messages.add
                body: constants.message('slot.remove.notempty')
                type: 'red'
                owner: info.cell
              return
            removeRow(info.i)
            return

        removeRecord = (info) ->
          saveRun info.cell, info.record.remove().then (done) ->
            unless done
              messages.add
                body: constants.message('record.remove.notempty')
                type: 'red'
                owner: info.cell
              return
            removeRow(info.i)
            return

        setRecord = (info, record) ->
          add = ->
            if record
              info.slot.addRecord(record)
            else if record != null
              info.slot.newRecord(info.c || '')
          act =
            if info.record
              info.slot.removeRecord(info.record).then(add)
            else
              add()

          saveRun info.cell, act.then (rr) ->
            record = rr?.record
            o = info.d
            if record
              r = populateRecordData(record)
              if o
                info.row.set(info.c, info.n, r)
              else
                info.n = info.row.add(info.c, r)
            else
              info.row.set(info.c, info.n, undefined)

            collapse()
            generateRow(info.i)
            expand(info) if info.n
            if record && setFocus == (i = info.id) && (i = document.getElementById(i)?.nextSibling) && (i = parseId(i))
              select(i)
            setFocus = undefined
            record

        updateDatum = (info, v) ->
          info.v = v
          info.d[info.metric.id] = v
          if info.category.fixed
            generateText(info)
          else
            for li in TBody.getElementsByClassName(info.p + info.d.id + '_' + info.metric.id)
              info.cell = li
              generateText(info)
          return

        saveDatum = (info, v) ->
          if info.c == 'slot'
            data = {}
            v = v == 'true' if info.metric.id == 'top'
            data[info.metric.id] = v ? ''
            return if `info.slot[info.metric.id] == v`
            saveRun info.cell, info.slot.save(data).then () ->
              updateDatum(info, v)
              return
          else if info.c == 'asset'
            data = {}
            t = info.metric.id
            data[t] = v ? ''
            return if info.asset[t] == data[t]
            saveRun info.cell, info.asset.save(data).then () ->
              updateDatum(info, v)
              return
          else
            return if info.record.measures[info.metric.id] == v
            saveRun info.cell, info.record.measureSet(info.metric.id, v).then () ->
              updateDatum(info, v)
              return

        ################################# Interaction

        # Collapse any expanded row.
        collapse = ->
          return unless Expanded
          i = Expanded.i
          tr = Expanded.tr
          Expanded = undefined
          tr.classList.remove('expand')
          t = 0
          while (el = tr.nextSibling) && el.data == i
            t++
            $(el).remove()

          el = tr.firstChild
          while el
            el.removeAttribute("rowspan")
            el = el.nextSibling

          t

        # Expand (or collapse) a row
        expand = (info) ->
          if Expanded?.i == info.i && `Expanded.c == info.c`
            if info.t == 'more'
              collapse()
            return
          collapse()

          return if info.i < 0
          Expanded = new Info(info.i)
          Expanded.c = info.c
          info.tr.classList.add('expand')

          max = Expanded.count
          max++ if Editing && Expanded.c != Key.id && Expanded.c != 'slot'
          return if max <= 1
          next = info.tr.nextSibling
          start = Expanded.count == 1
          for n in [+start..max-1] by 1
            Expanded.n = n
            Expanded.tr = TBody.insertBefore(document.createElement('tr'), next)
            Expanded.tr.data = Expanded.i
            Expanded.tr.className = 'expand'
            generateRecord(Expanded)
          Expanded.tr = info.tr

          max++ unless start
          el = info.tr.firstChild
          while el
            info = new Info(el)
            if `info.c != Expanded.c`
              el.setAttribute("rowspan", max)
            el = el.nextSibling
          return

        save = (info, type, value) ->
          if value == ''
            value = undefined
          else switch type
            when 'record'
              if value == 'new'
                setRecord(info)
              else if value == 'remove'
                setRecord(info, null) if info.d
              else if v = stripPrefix(value, 'add_')
                u = v.indexOf('_')
                info.metric = constants.metric[v.slice(0,u)]
                v = v.slice(u+1)
                setRecord(info).then (r) ->
                  info.record = r
                  saveDatum(info, v) if r
                  return
              else if !isNaN(v = parseInt(value, 10))
                if v != info.d?.id
                  setRecord(info, volume.records[v])
              return
            when 'options'
              # force completion of the first match
              # this completely prevents people from using prefixes of options but maybe that's reasonable
              c = optionCompletions(value) if value
              value = c[0] if c?.length

          if type == 'ident'
            r = editScope.identCompleter(value)
            r.find((o) -> o.default)?.run(info) if Array.isArray(r)
          else if info.i == -1
            return unless value
            createNew(info).then (row) ->
              saveDatum(new Info(row.tr.firstChild), value)
          else
            saveDatum(info, value)
          return

        editScope = $scope.$new(true)
        editScope.constants = constants
        editInput = editScope.input = {}
        editCellTemplate = $compile($templateCache.get('volume/spreadsheetEditCell.html'))
        editCell = undefined

        unedit = (event) ->
          return unless edit = editCell
          editCell = undefined
          cell = edit.parentNode
          $(edit).remove()
          return unless cell?.parentNode
          cell.classList.remove('editing')
          tooltips.clear()

          info = new Info(cell)
          save(info, editScope.type, editInput.value) if event != false
          info

        recordDescription = (r) ->
          k = Object.keys(r.measures)
          if k.length
            k.sort(byNumber).map((m) -> r.measures[m]).join(', ')
          else
            '[' + r.id + ']'

        edit = (info) ->
          switch info.t
            when undefined
              if info.c == 'asset'
                # for now, just go to slot edit
                $location.url(info.slot.editRoute({asset:info.d.id}))
                return
              return if info.row.top
              m = info.metric
              if m.name == 'indicator'
                # trash/bullet: remove
                setRecord(info, null) if info.category != Key
                return
              return if m.readonly
              editScope.type = m.type
              mi = m.id
              editScope.options = m.options
              if info.c == 'slot'
                return if info.slot?.top && (mi == 'date' || mi == 'release')
                v = info.slot?[mi]
                v = !!v if mi == 'top'
              else if info.c == 'asset' # not reached
                v = info.asset[mi]
              else
                v = info.record?.measures[mi]
                if info.col.first && info.category != Key
                  editScope.type = 'ident'
                  editScope.info = info
                  rs = []
                  mf = (r) -> (m) -> r.measures[m]
                  for ri, r of volume.records
                    if r.category == info.c && !info.row.list(info.c).some((d) -> `d.id == ri`)
                      rs.push
                        r:r
                        v:(r.measures[mi] ? '').toLowerCase()
                        d:recordDescription(r)
                  editScope.records = rs.sort((a, b) -> byMagic(a.v, b.v))
                else if m.options
                  editScope.type = 'options'
                else if m.long
                  editScope.type = 'long'
              editInput.value = (v ? '')+''
            when 'add'
              if info.i == -1
                createNew(info)
                return
              if info.c == 'asset'
                # for now, just go to slot edit
                $location.url(info.slot.editRoute())
                return
              return if info.row.top
              c = info.category
              editInput.value = (info.d?.id ? 'remove')+''
              editScope.type = 'record'
              editScope.options =
                new: 'Create new ' + c.name
                remove: c.not
              for ri, r of volume.records
                if r.category == c.id && (!info.row.list(info.c).some((d) -> d.id == ri) || ri == editInput.value)
                  editScope.options[ri] = r.displayName
              ms = info.cols.metrics
              # detect special cases: singleton or unitary records
              if ms.length == 1
                if ms[0].type == 'void' && Object.keys(editScope.options).length > 2
                  # singleton: id only, existing record(s)
                  delete editScope.options['new']
                else if ms[0].options
                  # unitary: single metric with options
                  delete editScope.options['new']
                  for o in ms[0].options
                    found = false
                    for ri, r of volume.records
                      if r.category == c.id && r.measures[m.id] == o
                        found = true
                        break
                    editScope.options['add_'+ms[0].id+'_'+o] = o unless found
            else
              return

          e = editCellTemplate editScope, (e) ->
            info.cell.insertBefore(editCell = e[0], info.cell.firstChild)
            info.cell.classList.add('editing')
            return
          e.on 'click', ($event) ->
            # prevent other ng-click handlers from taking over
            $event.stopPropagation()
            return

          tooltips.clear()
          $timeout ->
            input = e.find('[name=edit]')
            input.filter('input,textarea').focus().select()
            input.filter('select').focus().one('change', $scope.$lift(editScope.unedit))
            return
          return

        unselect = ->
          styles.clear()
          unedit()
          return

        $scope.$on '$destroy', unselect

        select = (info) ->
          unselect()
          expand(info)
          if !info.t
            for c in info.cell.classList when c.startsWith('ss-')
              styles.set('.' + c + '{background-color:' +
                (if c.includes('_', 4) then 'rgba(243,180,140,0.7)' else 'rgba(243,180,140,0.4)') +
                ';\n text-}')
              info.cell.classList.add('selected')

          edit(info) if Editing
          return

        $scope.click = (event) ->
          return unless info = parseId(event.target)

          select(info)
          if info.metric?.id == 'age'
            display.toggleAge()
          return

        doneEdit = (event, info) ->
          if info && event && event.$key == 'Tab'
            setFocus = !event.shiftKey && info.cell.id
            c = info.cell
            while true
              c = if event.shiftKey then c.previousSibling else c.nextSibling
              return unless c && i = parseId(c)
              break unless i.category?.id == 'asset' || i.metric?.name == 'indicator' # skip "delete" actions
            select(i)

          return

        editScope.unedit = (event) ->
          doneEdit(event, unedit(event))
          event.preventDefault() if event
          false

        editSelect = (event) ->
          editInput.value = @text
          editScope.unedit(event)
          return # inputCompleter needs an undefined to do nothing

        editScope.identCompleter = (input) ->
          info = editScope.info
          o = []
          defd = false
          add = (t, f, d) ->
            o.push
              text: t
              select: (event) ->
                info = unedit(false)
                f(info)
                doneEdit(event, info)
                return
              run: f
              default: d && !defd
            defd ||= d
          if info.d
            if input == info.record.measures[info.metric.id]
              add("Keep " + info.record.displayName,
                () -> return,
                true)
            if !input
              add("Remove " + info.record.displayName + " from this session",
                (info) -> setRecord(info, null),
                true)
          if !info.d || input && input != info.record.measures[info.metric.id]
            inputl = (input ? '').toLowerCase()
            set = (r) -> (info) ->
              setRecord(info, r)
            rs = (r for r in editScope.records when r.v.startsWith(inputl))
            for r in rs
              add("Use " + info.category.name + ' ' + r.d, set(r.r), input && rs.length == 1 || r.v == inputl)
            os = if info.metric.options
                (x for x in info.metric.options when x.toLowerCase().startsWith(inputl))
              else
                []
            if input && !os.length
              os = [input]
            os.forEach (i) ->
              if info.d
                add("Change all " + info.record.displayName + " " + info.metric.name + " to '" + i + "'",
                  (info) -> saveDatum(info, i),
                  input && !rs.length && os.length == 1 || i == input)
              add("Create new " + info.category.name + " with " + info.metric.name + " '" + i + "'",
                (info) -> setRecord(info).then((r) ->
                  info.record = r
                  saveDatum(info, i) if r
                  return),
                input && !rs.length && os.length == 1 || i == input)
          if o.length then o else input

        optionCompletions = (input) ->
          i = input.toLowerCase()
          (o for o in editScope.options when o.toLowerCase().startsWith(i))

        editScope.optionsCompleter = (input) ->
          match = optionCompletions(input)
          switch match.length
            when 0
              input
            when 1
              match[0]
            else
              ({text:o, select: editSelect, default: input && i==0} for o, i in match)

        $scope.clickMetric = sortBy

        clickRemove = (event) ->
          return unless info = parseId(event.target.parentNode)
          if info.c == 'slot'
            removeSlot(info)
          else if typeof info.c == 'number'
            removeRecord(info)
          event.stopPropagation()
          false

        $scope.unlimit = ->
          Limit = undefined
          fill()

        if Editing
          $document.on 'click', ($event) ->
            if editCell && editCell.parentNode != $event.target && !$.contains(editCell.parentNode, $event.target)
              $scope.$applyAsync(unedit)
            return

        $scope.tabOptionsClick = false
        $scope.tabOptionsToggle = ($event) ->
          $scope.tabOptionsClick = !$scope.tabOptionsClick
          $event.stopPropagation()
          false

        ################################# main

        $scope.setKey = (key) ->
          unedit()
          Key = $scope.key = key? && getCategory(key) || pseudoCategory.slot
          $location.replace().search('key', Key.id) unless Top
          populate()
          $scope.tabOptionsClick = undefined

        $scope.setKey($attrs.key || !Top && $location.search().key)
        return

    ]
    }
]
