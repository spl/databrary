'use strict'

app.factory 'tooltipService', [
  '$rootScope', '$timeout', '$document',
  ($rootScope, $timeout, $document) ->

    delay = 500
    list = {}

    namespace = '.tooltip'
    focusElements = {INPUT:true, SELECT:true, TEXTAREA:true}
    focusEvents = {in:'focusin'   +namespace, out:'focusout'  +namespace}
    mouseEvents = {in:'mouseenter'+namespace, out:'mouseleave'+namespace}

    events = (target) ->
      if focusElements[target.prop('tagName')]
        focusEvents
      else
        mouseEvents

    class BaseTooltip
      constructor: () ->
        list[@id] = @

      cancelTimeout: () ->
        return unless @timeout
        $timeout.cancel(@timeout)
        @timeout = undefined

      out: () ->
        @cancelTimeout()
        @active = undefined

      remove: () ->
        delete list[@id]

    class Tooltip extends BaseTooltip
      constructor: (@message) ->
        @target = $()
        super()

      @prototype.live = false

      Object.defineProperty @prototype, 'id',
        get: () -> @message

      add: (target) ->
        ev = events(target)
        target.on ev.in, (event) =>
          @cancelTimeout()
          @timeout = $timeout () =>
              @active = event.target
            , delay

        target.on ev.out, $rootScope.$lift () =>
          @out()

        @target = @target.add(target)
        return

      remove: (target) ->
        ev = events(target)
        target.off(ev.in + ' ' + ev.out)
        @cancelTimeout()
        @active = undefined if target.is(@active)

        @target = @target.not(target)
        unless @target.length
          super()
        return

    class LiveTooltip extends BaseTooltip
      constructor: (@target, @message) ->
        $document.on mouseEvents.in, @target, (event) =>
          @cancelTimeout()
          @timeout = $timeout () =>
              if document.body.contains(event.target) && $(event.target).is(@target) # may have changed
                @active = event.target
            , delay

        $document.on mouseEvents.out, @target, $rootScope.$lift () =>
          @out()

        super()
      
      @prototype.live = true

      Object.defineProperty @prototype, 'id',
        get: () -> @target

    {
      list: list
      add: (target, message) ->
        if typeof target == 'string'
          if target of list
            throw new Error('duplicate LiveTooltip')
          new LiveTooltip(target, message)
        else
          cur = list[message] || new Tooltip(message)
          cur.add(target)
          cur
      clear: () ->
        for i, t of list
          t.out()
        return
    }
]
