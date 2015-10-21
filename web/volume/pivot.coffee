'use strict'

app.directive 'volumePivot', [
  '$sanitize', 'constantService', 'displayService',
  ($sanitize, constants, display) ->
    restrict: 'E'
    link: ($scope, $element) ->
      pivot = $scope.pivot

      indicator = constants.metricName.indicator.id

      pivot.run = (rows, opts) ->
        cols = $scope.groups
        head = []
        for g in cols when g.category.id != 'asset'
          n = g.category.name + ' '
          for m in g.metrics when m.id != 'summary'
            if m.id == 'age'
              agemode = display.ageMode($scope.volume.summary.agemean)
              head.push(n + m.name + ' (' + agemode + 's)')
              agediv = constants.age[agemode]
            else
              head.push(n + m.name)
        data = [head]

        disp = (m, v) ->
          return '' unless v?
          switch m.id
            when 'release'
              constants.release[v]
            when 'age'
              v / agediv
            when 'top'
              m.options[v]
            when indicator
              true
            else
              $sanitize(v)

        for row in rows when row.filt
          data.push(d = [])
          for g in cols when g.category.id != 'asset'
            l = row.list(g.category.id)
            switch l.length
              when 0
                for m in g.metrics when m.id != 'summary'
                  d.push('<em>none</em>')
              when 1
                r = l[0]
                for m in g.metrics when m.id != 'summary'
                  d.push(disp(m, r[m.id]))
              else
                for m in g.metrics when m.id != 'summary'
                  a = _.uniq(i[m.id] for i in l when i[m.id]?)
                  switch a.length
                    when 0
                      d.push(undefined)
                    when 1
                      d.push(disp(m, a[0]))
                    else
                      d.push('<em>multiple</em>')
                
        $element.pivotUI(data, opts, opts?)
        @active = true
        return

      pivot.clear = ->
        @active = false
        $element.empty()

      pivot.get = ->
        return unless @active
        opts = $element.data('pivotUIOptions')
        r = {}
        for k in ['rendererName', 'cols', 'rows', 'aggregatorName', 'vals']
          if k of opts
            r[k] = opts[k]
        r

      pivot.init?()

      return
]
