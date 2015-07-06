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
      id:
        id: 'id'
        name: 'id'
        display: ' '
        type: 'number'
        release: constants.release.PUBLIC
        sort: -10000
        readonly: true
      name: # slot, asset
        id: 'name'
        name: 'name'
        display: ' '
        type: 'text'
        sort: -9000
      date: # slot
        id: 'date'
        name: 'test date'
        type: 'date'
        sort: -8000
      release: # slot
        id: 'release'
        name: 'release'
        type: 'release'
        sort: -7000
      age: # record
        id: 'age'
        name: 'age'
        type: 'number'
        release: constants.release.EXCERPTS
        sort: constants.metricName.birthdate.id + 0.5
        readonly: true
      summary: # slot
        id: 'summary'
        name: 'summary'
        display: ' '
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
        Top = $scope.top = 'top' of $attrs
        ID = $scope.id = $attrs.id ? if Top then 'sst' else 'ss'
        Limit = $attrs.limit
        Key = undefined

        ###
        # We use the following types of data structures:
        #   Row = index of slot in slots and rows (i)
        #   Data[Row] = scalar value (array over Row)
        #   Slot_id = Database id of container
        #   Segment = standard time range (see type service)
        #   Record_id = Database id of record
        #   Category_id = Database id of record category (c)
        #   Count = index of record within category for slot (n)
        #   Metric_id = Database id of metric, or "id" for Record_id, or "age" (m)
        ###

        Order = []      # Permutation Array of Row in display order
        Data = {}       # [Category_id][Metric_id][Count] :: Data
        Counts = []     # [Row][Category_id] :: Count
        Groups = []     # [] Array over categories :: {category: Category, metrics[]: Array of Metric}
        Cols = []       # [] Array over metrics :: {category: Category, metric: Metric} (flattened version of Groups)
        Depends = {}    # [Record_id][Row] :: Count
        Rows = []       # [Row] :: DOM Element tr

        TBody = $element[0].getElementsByTagName("tbody")[0]
        TFoot = $element[0].getElementsByTagName("tfoot")[0]

        pseudoCategory =
          slot:
            id: 'slot'
            name: if Top then 'materials' else 'session'
            not: 'No ' + (if Top then 'materials' else 'sessions')
            template: if Top then ['name'] else ['name', 'date', 'release']
            sort: -10000
            fixed: true
          0:
            id: 0
            name: 'record'
            not: 'No record'
            template: [constants.metricName.ID.id]
          asset:
            id: 'asset'
            name: 'file'
            not: 'No files'
            template: ['name']
            sort: 10000
            fixed: true
        constants.deepFreeze(pseudoCategory)
        getCategory = (c) ->
          pseudoCategory[c || 0] || constants.category[c]

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
          #   d: Data for id metric
          #   record: Record
          #   asset: Asset
          #   v: Data value

          constructor: (@cell) ->
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
            count: ->
              Counts[@i][@c] || 0
            metric: ->
              @col?.metric
            row: ->
              Rows[@i]
            d: ->
              Data[@c].id[@n]?[@i]
            p: ->
              cls = 'ss-'
              if typeof (c = @c) != 'number'
                cls += c.charAt(0)
              cls
            slot: ->
              volume.containers[if @c == 'slot' then @d else Data.slot.id[0][@i]]
            record: ->
              volume.records[@d]
            asset: ->
              @slot?.assets[@d]
            v: ->
              Data[@c][@m]?[@n]?[@i]

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

        populateDatum = (i, c, n, m, v) ->
          arr(arr(Data[c] ||= {id:[]}, m), n)[i] = v
          return

        populateSlotData = (i, n, slot) ->
          populateDatum(i, 'slot', n, 'id', slot.id)
          populateDatum(i, 'slot', n, 'name', slot.name)
          if !slot.top || slot.date
            populateDatum(i, 'slot', n, 'date', slot.date)
          if !slot.top || slot.release
            populateDatum(i, 'slot', n, 'release', slot.release+'')
          return

        populateRecordData = (i, n, record) ->
          c = record.category || 0
          populateDatum(i, c, n, 'id', record.id)
          for m, v of record.measures
            populateDatum(i, c, n, m, v)

        populateAssetData = (i, n, asset) ->
          populateDatum(i, 'asset', n, 'id', asset.id)
          populateDatum(i, 'asset', n, 'name', asset.name)
          return

        # Fill all Data values for Row i
        populateSlot = (i, slot) ->
          count = Counts[i] = {slot: 1}
          populateSlotData(i, 0, slot)

          for rr in slot.records
            record = rr.record
            # temporary workaround for half-built volume inclusions:
            continue unless record
            c = record.category || 0

            # populate depends:
            if Depends[record.id]
              # skip duplicates:
              continue if i of Depends[record.id]
            else
              Depends[record.id] = {}

            # determine Count:
            n = inc(count, c)

            populateRecordData(i, n, record)
            if !Editing && 'age' of rr
              populateDatum(i, c, n, 'age', rr.age)

            Depends[record.id][i] = n

          count.asset = 0
          for assetId, asset of slot.assets
            n = count.asset++
            populateAssetData(i, n, asset)

          return

        populateSlots = () ->
          i = 0
          for ci, slot of volume.containers when Top != !slot.top
            populateSlot(i++, slot)
          i

        populateRecord = (i, record) ->
          Counts[i] = {slot: 0}
          Counts[i][record.category || 0] = 1

          populateRecordData(i, 0, record)

        populateRecords = () ->
          i = 0

          records = {}
          for r, record of volume.records when (record.category || 0) == Key.id
            populateRecord(i, record)
            records[r] = i++
          Counts[count = i] = {slot: 0}
          Counts[i][Key.id] = 0

          for s, slot of volume.containers when Top != !slot.top
            deps = Depends[slot.id] = {}
            any = false
            for rr in slot.records when (i = records[rr.id])?
              n = Counts[i].slot++
              populateSlotData(i, n, slot)
              if !Editing && 'age' of rr
                populateDatum(i, 'slot', n, 'age', rr.age)
              populateDatum(i, 'slot', n, 'summary', (rrr.record.displayName for rrr in slot.records when rrr.id != rr.id).join(', '))
              deps[i] = n
              any = true
            unless any
              n = Counts[count].slot++
              populateSlotData(count, n, slot)
              populateDatum(count, 'slot', n, 'summary', (rrr.record.displayName for rrr in slot.records).join(', '))
              deps[count] = n

          count+!!Counts[count].slot

        # Fill Cols and Groups from records
        populateCols = ->
          Cols = []
          cats = Object.keys(Data).map(getCategory)
          cats.remove(Key)
          cats.sort(bySortId)
          cats.unshift(Key)
          $scope.groups = Groups = cats.map (category) ->
            d = Data[category.id]
            if Editing
              for m in category.template
                arr(d, m)

            metrics = Object.keys(d).map(getMetric).sort(bySortId)
            if metrics.length > 1
              metrics.shift() # remove 'id' (necessarily first)
            si = Cols.length
            Cols.push.apply Cols, _.map metrics, (m) ->
              category: category
              metric: m
              sortable: m.id != 'id' || metrics.length == 1
            l = metrics.length
            Cols[si].first = Cols[si+l-1].last = l
            {
              category: category
              metrics: metrics
              start: si
            }
          $scope.cols = Cols
          if Key.id == 'slot'
            $scope.views = (g.category for g in Groups when g.category.id != 'asset')
            if Editing
              $scope.categories = (c for ci, c of constants.category when ci not of Data)
              $scope.categories.sort(bySortId)
              $scope.categories.push(pseudoCategory[0]) unless 0 of Data
              Array.prototype.push.apply($scope.views, $scope.categories)
          else
            $scope.categories = []
          return

        # Call all populate functions
        populate = ->
          Data = {}
          Data[Key.id] = {id:[]}
          Depends = {}
          Counts = []
          if Key.id == 'slot'
            Data.asset = {id:[]}
            n = populateSlots()
          else
            n = populateRecords()
          populateCols()
          if Order.length != n
            Order = if n then [0..n-1] else []
          generate()
          return

        ################################# Generate HTML

        # Add or replace the text contents of cell c for measure/type m with value v
        generateText = (info) ->
          $(cell = info.cell).empty()
          v = info.v
          stop = info.slot?.id == volume.top.id
          if info.col.first && info.d?
            if info.c == 'asset'
              a = cell.appendChild(document.createElement('a'))
              icon = a.appendChild(document.createElement('img'))
              icon.src = info.asset.icon
              icon.className = "format hint-format-" + info.asset.format.extension
              t = {asset:info.d}
              a.setAttribute('href', if Editing then info.slot.editRoute(t) else info.slot.route(t))
              icon = cell.appendChild(document.createElement('span'))
              icon.className = 'icon release ' + constants.release[info.asset.release] + ' hint-release-' + constants.release[info.asset.release] 
            else
              if Editing && Key.id == info.c && !stop
                del = cell.appendChild(document.createElement('a'))
                del.className = 'trash icon'
                i = new Info()
                i.i = info.i
                i.c = info.c
                i.cell = cell
                $(del).on 'click', $scope.$lift(clickRemove)
              if info.c == 'slot'
                a = cell.appendChild(document.createElement('a'))
                a.className = "session icon hint-action-slot"
                a.setAttribute('href', if Editing then info.slot.editRoute() else info.slot.route())
          switch info.metric.id
            when 'name'
              if stop && info.c == 'slot'
                cell.classList.add('top-level-materials')
                v ?= constants.message('materials.top')
              else
                v ?= ''
            when 'release'
              cn = constants.release[v]
              cell.className = cn + ' release icon hint-release-' + cn
              v = ''
            when 'id'
              if v?
                cell.className = 'icon ' + if Editing && Key.id != info.c then 'trash' else 'bullet'
                v = ''
            when 'age'
              v = display.formatAge(v)
          if info.metric.long
            cell.classList.add('white-space-pre')
          if v?
            cell.classList.remove('blank')
          else
            cell.classList.add('blank')
            v = info.metric.assumed || ''
          cell.appendChild(document.createTextNode(v))
          cell.id = info.id
          if info.d?
            cell.classList.add(cls = info.p + info.d)
            cell.classList.add(cls + '_' + info.metric.id)
          return

        # Add a td element to tr r with value c and id i
        generateCell = (info) ->
          info.cell = info.row.appendChild(document.createElement('td'))
          if info.v == null
            info.cell.className = 'null'
          else
            generateText(info)
          return

        generateAdd = (info, td) ->
          width = info.cols.metrics.length
          info.m = info.cols.start
          if typeof info.metric.id == 'number'
            info.id = ID+'-'+info.i+'_'+info.cols.start+(if info.hasOwnProperty('n') then '_'+info.n else '')
            info.v = undefined
            info.d = undefined
            generateCell(info)
            info.row.insertBefore(info.cell, td)
            if width > 1
              td.setAttribute("colspan", width-1)
              td.appendChild(document.createTextNode("\u2190 add " + info.category.name))
            else
              info.row.removeChild(td)
          else
            td.classList.add('add')
            td.id = ID + '-add_' + info.i + '_' + info.c
            td.appendChild(document.createTextNode("add " + info.category.name))

        generateMultiple = (info) ->
          t = info.count
          return if (if info.hasOwnProperty('n') then info.n < t else t == 1)
          td = info.row.appendChild(document.createElement('td'))
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
          ms = info.cols.metrics
          return unless l = ms.length
          t = info.count
          r = Data[info.c]
          if td = generateMultiple(info)
            unless info.hasOwnProperty('n')
              cls = info.p
              for n in [0..t-1] by 1
                td.classList.add(cls + r.id[n][info.i])
            return
          pre = ID + '-' + info.i + '_'
          post = if info.hasOwnProperty('n') then '_' + info.n else ''
          for mi in [0..l-1] by 1
            info.m = info.cols.start+mi
            info.v = r[info.metric.id][info.n]?[info.i]
            info.d = r.id[info.n][info.i]
            info.id = pre + (info.cols.start+mi) + post
            generateCell(info)
          return

        # Fill out rows[i].
        generateRow = (i) ->
          info = new Info()
          info.i = i
          row = if Rows[i]
              $(Rows[i]).empty()
              Rows[i]
            else
              Rows[i] = document.createElement('tr')
          row.id = ID + '_' + i
          row.data = i
          if Editing && info.slot?.id == volume.top.id
            row.className = 'top'

          for col in Groups
            info.category = (info.cols = col).category
            generateRecord(info)
          return

        generateFoot = ->
          info = new Info()
          info.i = -1
          unless info.row
            Rows[info.i] = TFoot.appendChild(document.createElement('tr'))
            info.row.id = ID + '_add'
          $(info.row).empty()
          info.cols = Groups[0]
          info.category = Key
          td = info.row.appendChild(document.createElement('td'))
          td.setAttribute("colspan", info.cols.metrics.length)
          td.className = 'null'
          generateAdd(info, td)

        # Update all age displays.
        $scope.$on 'displayService-toggleAge', ->
          info = new Info()
          for m, mi in Cols
            continue unless m.metric.id == 'age'
            info.m = mi
            r = Data[info.c][info.metric.id]
            pre = ID + '-'
            mid = '_' + mi
            if expanded?.c == info.c && expanded.count > 1
              info.i = expanded.i
              premid = pre + info.i + mid + '_'
              for n in [0..expanded.count-1] by 1 when n of r
                info.n = n
                info.v = r[n][info.i]
                info.cell = document.getElementById(premid + n)
                generateText(info) if info.cell
            return unless 0 of r
            r = r[0]
            for d, i in r
              if Counts[i][info.c] == 1
                info.i = i
                info.v = d
                info.cell = document.getElementById(pre + i + mid)
                generateText(info) if info.cell

        # Generate all rows.
        generate = ->
          for i, n in Order
            generateRow(i)
          fill()
          if Rows.length > Order.length
            foot = Rows[-1]
            for r in Rows.splice(Order.length)
              TBody.removeChild(r) if r.parentNode
            Rows[-1] = foot
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
              TBody.removeChild(Rows[i]) if Rows[i].parentNode
            else
              TBody.appendChild(Rows[i])
          return

        # Populate order based on compare function applied to values.
        sort = (values, compare) ->
          return unless values
          compare ?= byMagic
          idx = new Array(Order.length)
          for o, i in Order
            idx[o] = i
          Order.sort (i, j) ->
            compare(values[i], values[j]) || idx[i] - idx[j]
          return

        currentSort = undefined
        currentSortDirection = false

        # Sort by column
        sortBy = (col, compare) ->
          values = Data[col.category.id][col.metric.id][0]
          if currentSort == col
            currentSortDirection = !currentSortDirection
            Order.reverse()
          else
            sort(values, compare)
            currentSort = col
            currentSortDirection = false
          fill()
          return

        $scope.colClasses = (col) ->
          cls = []
          if typeof col == 'object'
            cls.push 'first' if col.first
            cls.push 'last' if col.last
            cls.push 'sort' if col.sortable
          else
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
              res
            , (res) ->
              cell.classList.remove('saving')
              cell.classList.add('error')
              messages.addError
                body: 'Error saving data' # FIXME
                report: res
                owner: cell
              return

        addRow = (info, i) ->
          Order.push(i)
          if info.i == -1
            Rows[i] = Rows[-1]
            delete Rows[-1]
            generateFoot()
          info.i = i
          generateRow(i)
          TBody.appendChild(Rows[i])
          info

        createSlot = (info) ->
          saveRun info.cell, volume.createContainer({top:Top}).then (slot) ->
            arr(slot, 'records')
            i = Rows.length
            populateSlot(i, slot)
            addRow(info, i)

        createRecord = (info) ->
          saveRun info.cell, volume.createRecord(info.c || undefined).then (record) ->
            i = Rows.length
            populateRecord(i, record)
            addRow(info, i)

        createNew = (info) ->
          if info.c == 'slot'
            createSlot(info)
          else if typeof info.c == 'number'
            createRecord(info)

        removeRow = (i) ->
          unedit(false)
          collapse()
          $(Rows[i]).remove()
          Counts.splice(i, 1)
          foot = Rows[-1]
          Rows.splice(i, 1)
          Rows[-1] = foot
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
              r = record.id
              info.n = inc(Counts[info.i], info.c) unless info.record

              for m, rcm of Data[info.c]
                v = if m of record then record[m] else record.measures[m]
                if v == undefined
                  delete rcm[info.n][info.i] if info.n of rcm
                else
                  arr(rcm, info.n)[info.i] = v
              # TODO this may necessitate regenerating column headers
            else
              t = --Counts[info.i][info.c]
              for m, rcm of Data[info.c]
                for n in [info.n+1..rcm.length-1] by 1
                  arr(rcm, n-1)[info.i] = arr(rcm, n)[info.i]
                delete rcm[t][info.i] if t of rcm

            delete Depends[o][info.i] if o?
            obj(Depends, r)[info.i] = info.n if record

            collapse()
            generateRow(info.i)
            expand(info) if info.n
            if record && setFocus == (i = info.id) && (i = document.getElementById(i)?.nextSibling) && (i = parseId(i))
              select(i)
            setFocus = undefined
            record

        updateDatum = (info, v) ->
          info.v = v
          rcm = Data[info.c][info.metric.id]
          if info.c == Key.id || info.c == 'asset'
            arr(rcm, info.n)[info.i] = v
            generateText(info)
          else
            for i, n of Depends[info.d]
              arr(rcm, n)[i] = v
              # TODO age may have changed... not clear how to update.
            l = TBody.getElementsByClassName(info.p + info.d + '_' + info.metric.id)
            for li in l
              info.cell = li
              generateText(info)
          return

        saveDatum = (info, v) ->
          if info.c == 'slot'
            data = {}
            data[info.metric.id] = v ? ''
            return if info.slot[info.metric.id]+'' == v
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

        expanded = undefined # Info

        # Collapse any expanded row.
        collapse = ->
          return unless expanded
          i = expanded.i
          expanded = undefined
          row = Rows[i]
          row.classList.remove('expand')
          t = 0
          while (el = row.nextSibling) && el.data == i
            t++
            $(el).remove()

          el = row.firstChild
          while el
            el.removeAttribute("rowspan")
            el = el.nextSibling

          t

        # Expand (or collapse) a row
        expand = (info) ->
          if expanded?.i == info.i && `expanded.c == info.c`
            if info.t == 'more'
              collapse()
            return
          collapse()

          return if info.i < 0
          expanded = new Info()
          expanded.i = info.i
          expanded.c = info.c
          info.row.classList.add('expand')

          max = expanded.count
          max++ if Editing && expanded.c != Key.id && expanded.c != 'slot'
          return if max <= 1
          next = info.row.nextSibling
          start = expanded.count == 1
          for n in [+start..max-1] by 1
            expanded.n = n
            expanded.row = TBody.insertBefore(document.createElement('tr'), next)
            expanded.row.data = expanded.i
            expanded.row.className = 'expand'
            generateRecord(expanded)
          expanded.row = info.row

          max++ unless start
          el = info.row.firstChild
          while el
            info = new Info(el)
            if `info.c != expanded.c`
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
                setRecord(info, null) if info.d?
              else if v = stripPrefix(value, 'add_')
                u = v.indexOf('_')
                info.metric = constants.metric[v.slice(0,u)]
                v = v.slice(u+1)
                setRecord(info).then (r) ->
                  info.record = r
                  saveDatum(info, v) if r
                  return
              else if !isNaN(v = parseInt(value, 10))
                if v != info.d
                  setRecord(info, volume.records[v])
              return
            when 'metric'
              if value != undefined
                arr(Data[info.c], value)
                populateCols()
                generate()
              return
            when 'category'
              if value != undefined
                arr(obj(Data, value), 'id')
                populateCols()
                generate()
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
            createNew(info).then (info) ->
              info.cell = info.row.firstChild
              saveDatum(info, value)
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
                $location.url(info.slot.editRoute({asset:info.d}))
                return
              return if info.slot?.id == volume.top.id
              m = info.metric.id
              if m == 'id'
                # trash/bullet: remove
                setRecord(info, null) if info.category != Key
                return
              return if info.metric.readonly
              editScope.type = info.metric.type
              if info.c == 'slot'
                v = info.slot?[m]
                v += '' if m == 'release'
              else if info.c == 'asset' # not reached
                v = info.asset[m]
              else
                v = info.record?.measures[m]
                if info.col.first && info.category != Key
                  editScope.type = 'ident'
                  editScope.info = info
                  rs = []
                  mf = (r) -> (m) -> r.measures[m]
                  for ri, r of volume.records
                    if (r.category || 0) == info.c && !Depends[ri]?[info.i]
                      rs.push
                        r:r
                        v:(r.measures[info.metric.id] ? '').toLowerCase()
                        d:recordDescription(r)
                  editScope.records = rs.sort((a, b) -> byMagic(a.v, b.v))
                else if info.metric.options
                  editScope.type = 'options'
                  editScope.options = info.metric.options
                else if info.metric.long
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
              return if info.slot?.id == volume.top.id
              c = info.category
              editInput.value = (info.d ? 'remove')+''
              editScope.type = 'record'
              editScope.options =
                new: 'Create new ' + c.name
                remove: c.not
              for ri, r of volume.records
                if (r.category || 0) == c.id && (!Depends[ri]?[info.i] || ri == editInput.value)
                  editScope.options[ri] = r.displayName
              # detect special cases: singleton or unitary records
              for mi of Data[c.id]
                mm = constants.metric[mi]
                if !m
                  m = mm
                else if mm
                  m = null
                  break
              if m == undefined && Object.keys(editScope.options).length > 2
                # singleton: id only, existing record(s)
                delete editScope.options['new']
              else if m && m.options
                # unitary: single metric with options
                delete editScope.options['new']
                for o in m.options
                  found = false
                  for ri, r of volume.records
                    if (r.category || 0) == c.id && r.measures[m.id] == o
                      found = true
                      break
                  editScope.options['add_'+m.id+'_'+o] = o unless found
            when 'category'
              editScope.type = 'metric'
              editInput.value = undefined
              editScope.options = []
              for mi, m of constants.metric when !(mi of Data[info.c])
                editScope.options.push(m)
              editScope.options.sort(bySortId)
            when 'head'
              editScope.type = 'category'
              editInput.value = undefined
              editScope.options = $scope.categories
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
                (if c.includes('_', 4) then 'rgba(226,217,0,0.6)' else 'rgba(242,238,100,0.4)') +
                ';\n text-}')

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
              break unless !info.t && info.metric.id == 'id' # skip "delete" actions
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
          if info.d?
            if input == info.record.measures[info.metric.id]
              add("Keep " + info.record.displayName,
                () -> return,
                true)
            if !input
              add("Remove " + info.record.displayName + " from this session",
                (info) -> setRecord(info, null),
                true)
          if !info.d? || input && input != info.record.measures[info.metric.id]
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
              if info.d?
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

        $scope.clickAdd = ($event) ->
          unselect()
          edit({cell:$event.target, t:'head'})
          false

        $scope.clickCategoryAdd = ($event, col) ->
          unselect()
          edit({cell:document.getElementById(ID+'-category_'+col.category.id), t:'category', c:col.category.id})
          $event.stopPropagation()
          $scope.tabOptionsClick = undefined
          false

        $scope.clickMetric = (col) ->
          sortBy(col) if col.sortable
          false

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
          Key = $scope.key = key? && getCategory(key) || pseudoCategory.slot
          unedit()
          collapse()
          populate()
          $scope.tabOptionsClick = undefined

        $scope.setKey($attrs.key)
        return


    ]
    }
]
