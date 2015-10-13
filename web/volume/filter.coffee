'use strict'

app.directive 'slotFilter', [
  'constantService',
  (constants) ->
    restrict: 'E'
    templateUrl: 'volume/filter.html'
    link: ($scope) ->
      join = (o, op, e) ->
        if o then o + op + e else e
      ops =
        '': (o) -> o
        any: (o) -> join(o, '&&', 'true')
        true: (o) -> join(o, '||', 'v')
        false: (o) -> join(o, '||', '!v')
        eq: (o,v) -> join(o, '||', 'v=='+JSON.stringify(v))
        ne: (o,v) -> join(o, '&&', 'v!='+JSON.stringify(v))
        gt: (o,v) -> join(o, '&&', 'v>'+JSON.stringify(v))
        ge: (o,v) -> join(o, '&&', 'v>='+JSON.stringify(v))
        lt: (o,v) -> join(o, '&&', 'v<'+JSON.stringify(v))
        le: (o,v) -> join(o, '&&', 'v<='+JSON.stringify(v))

      $scope.filter.makeFilter = () ->
        exp = ['var v,c,i,r']
        exp.push('if(slot.id==='+$scope.volume.top.id+')return')
        cats = {slot:{}}
        ci = 0
        for f in @list when f.op
          c = f.category.id
          unless c of cats
            cats[c] = {index:ci++}
          m = f.metric.id
          cats[c][m] = ops[f.op](cats[c][m], f.value)
        for m, e of cats.slot when e
          exp.push('v=slot.'+m)
          exp.push('if(!('+e+'))return')
        if ci
          exp.push(
            'c=new Uint8Array(new ArrayBuffer('+ci+'))',
            'for(i=0;i<slot.records.length;i++){if(!(r=slot.records[i].record)){continue')
          for c, mets of cats when c != 'slot'
            exp.push('}else if(r.category==='+c+'){if(c['+mets.index+'])continue')
            for m, e of mets when m != 'index' && e
              if m == 'age'
                exp.push('v=slot.records[i].age')
              else
                exp.push('v=r.measures['+m+']')
                if constants.metric[m].assumed
                  exp.push('if(v==null)v='+JSON.stringify(constants.metric[m].assumed))
                exp.push('if(!('+e+'))continue')
            exp.push('c['+mets.index+']=1')
          exp.push('}}for(i=0;i<'+ci+';i++)if(!c[i])return')
        exp.push('return true;')
        #console.log(exp.join(';'))
        new Function('slot', exp.join(';'))

      $scope.filter.remove = (i) ->
        @list.splice(i, 1)
        $scope.filter.change()

      $scope.filter.change = $scope.filter.update

      return
]
