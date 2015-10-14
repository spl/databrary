'use strict'

app.directive 'slotFilter', [
  'constantService',
  (constants) ->
    restrict: 'E'
    templateUrl: 'volume/filter.html'
    link: ($scope) ->
      filter = $scope.filter

      join = (o, op, e) ->
        if o then o + op + e else e
      ops = # these are all negated
        '': (o) -> o
        any: (o) -> o || 'false'
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
        exp = ['var v']
        cats = {slot:{}}
        ci = 0
        for f in filter.list when f.op
          c = f.category.id
          unless c of cats
            cats[c] =
              $index:ci++
          m = f.metric.id
          cats[c][m] = ops[f.op](cats[c][m], f.value)

        record = (mets, rv, age, brk) ->
          any = false
          for m, e of mets when e && m != '$index' && `m != indicator`
            any = true
            if m == 'age'
              exp.push('v='+age)
            else
              exp.push('v='+rv+'.measures['+m+']')
              if constants.metric[m].assumed
                exp.push('if(v==null)v='+JSON.stringify(constants.metric[m].assumed))
            exp.push('if('+e+')'+brk)
          mets[indicator] = ops[if any then 'two' else 'any'](mets[indicator])
          any

        if filter.key && filter.key != 'slot'
          if mets = cats[filter.key]
            exp.push('if(s){')
            exp.push('v='+(if record(mets, 's', 'undefined', 'return') then 2 else 1))
            exp.push('}else v=0',
              'if('+mets[indicator]+')return')
        else
          for m, e of cats.slot when e
            exp.push('v=s.'+m,
              'if('+e+')return')
          if ci
            exp.push('var c,i,r')
            exp.push('c=new Uint8Array(new ArrayBuffer('+ci+'))',
              'for(i=0;i<s.records.length;i++){if(!(r=s.records[i].record)){continue')
            for c, mets of cats when c != 'slot'
              exp.push('}else if(r.category==='+c+'){c['+mets.$index+']=1')
              if record(mets, 'r', 's.record[i].age', 'continue')
                exp.push('c['+mets.$index+']=2')
            exp.push('}}')
            for c, mets of cats when c != 'slot'
              exp.push('v=c['+mets.$index+']',
                'if('+mets[indicator]+')return')
        exp.push('return true;')
        exp = exp.join(';')
        console.log(exp) if DEBUG
        exp

      filter.make = () ->
        new Function('s', makeFilter())

      old = undefined
      filter.change = () ->
        f = makeFilter()
        return if f == old
        old = f
        filter.update(new Function('s', f))
        return

      filter.remove = (i) ->
        @list.splice(i, 1)
        filter.change()

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
              filter.change()
              this.text
            default: input && i==0

      return
]
