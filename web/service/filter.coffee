'use strict'

app.factory 'slotFilter', [
  'constantService',
  (constants) ->

    exact = (v, c) ->
      v == undefined || if Array.isArray(v) then c in v else `c == v`
    types =
      options: exact
      text: exact
      numeric: exact
      date: exact

    class Filter
      constructor: () ->
        return

      set: (c, m, v) ->
        if m == undefined
          delete this[c]
        else if m == null
          this[c] = m
        else
          unless this[c]
            this[c] = {}
          if v == undefined
            delete this[c][m]
          else
            this[c][m] = v
        return

      apply: (slot) ->
        (!@slot ||
          exact(@slot.top, slot.top || false) &&
          types.text(@slot.name, slot.name) &&
          types.date(@slot.date, slot.date) &&
          types.numeric(@slot.release, slot.release)) &&
        constants.categories.every (cat) =>
          c = this[cat.id]
          unless c
            return c == undefined || slot.records.every (sr) ->
              sr.record?.category != c
          slot.records.some (sr) ->
            r = sr.record
            return unless r?.category == cat.id
            for mi, mv of c
              if mi == 'age'
                t = 'numeric'
                v = sr.age
              else
                m = constants.metric[mi]
                t = if m.options then 'options' else m.type
                v = r.measures[mi] ? m.assumed
              return false unless types[t](mv, v)
            true

    Filter
]
