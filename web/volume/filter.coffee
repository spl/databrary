'use strict'

app.directive 'slotFilter', [
  'constantService',
  (constants) ->
    restrict: 'E'
    templateUrl: 'volume/filter.html'
    link: ($scope) ->
      join = (o, op, e) ->
        if o then o + op + e else e
      ops = # these are all negated
        '': (o) -> o
        any: (o) -> o || 'true'
        true: (o) -> join(o, '&&', '!v')
        false: (o) -> join(o, '&&', 'v')
        two: (o) -> join(o, '&&', 'v!==2')
        eq: (o,x) -> join(o, '&&', if x then 'v!='+JSON.stringify(x) else 'v')
        ne: (o,x) -> join(o, '||', 'v=='+JSON.stringify(x))
        gt: (o,x) -> join(o, '||', 'v<='+JSON.stringify(x))
        ge: (o,x) -> join(o, '||', 'v<'+JSON.stringify(x))
        lt: (o,x) -> join(o, '||', 'v>='+JSON.stringify(x))
        le: (o,x) -> join(o, '||', 'v>'+JSON.stringify(x))
        contains: (o,v,x) -> join(o, '||', '!('+v+'&&'+v+'.includes('+JSON.stringify(x)+'))')
      indicator = constants.metricName.indicator.id

      makeFilter = () ->
        exp = ['var v,c,i,r']
        exp.push('if(s.id==='+$scope.volume.top.id+')return')
        cats = {slot:{}}
        ci = 0
        for f in $scope.filter.list when f.op
          c = f.category.id
          unless c of cats
            cats[c] =
              $index:ci++
          m = f.metric.id
          cats[c][m] = ops[f.op](cats[c][m], f.value)
        for m, e of cats.slot when e
          exp.push('v=s.'+m,
            'if('+e+')return')
        if ci
          exp.push('c=new Uint8Array(new ArrayBuffer('+ci+'))',
            'for(i=0;i<s.records.length;i++){if(!(r=s.records[i].record)){continue')
          for c, mets of cats when c != 'slot'
            exp.push('}else if(r.category==='+c+'){c['+mets.$index+']=1')
            any = false
            for m, e of mets when e && m != '$index' && `m != indicator`
              any = true
              if m == 'age'
                exp.push('v=s.records[i].age')
              else
                exp.push('v=r.measures['+m+']')
                if constants.metric[m].assumed
                  exp.push('if(v==null)v='+JSON.stringify(constants.metric[m].assumed))
              exp.push('if('+e+')continue')
            if any
              exp.push('c['+mets.$index+']=2')
              cats[c][indicator] = ops.two(cats[c][indicator])
          exp.push('}}')
          for c, mets of cats when c != 'slot'
            exp.push('v=c['+mets.$index+']',
              'if('+mets[indicator]+')return')
        exp.push('return true;')
        exp = exp.join(';')
        console.log(exp) if DEBUG
        exp

      $scope.filter.remove = (i) ->
        @list.splice(i, 1)
        $scope.filter.change()

      old = undefined
      $scope.filter.change = () ->
        f = makeFilter()
        return if f == old
        old = f
        $scope.filter.update(new Function('s', f))
        return

      $scope.filterCompleter = (f, input) ->
        i = input.toLowerCase()
        match = (o for o in f.metric.options when o.toLowerCase().startsWith(i))
        switch match.length
          when 0 then input
          when 1 then match[0]
          else for o, i in match
            text: o
            select: () ->
              f.value = this.text
              $scope.filter.change()
              this.text
            default: input && i==0

      return
]
