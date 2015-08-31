'use strict'

app.controller 'party/profile', [
  '$rootScope', '$scope', '$location', '$filter', 'displayService', 'constantService', 'modelService', 'messageService', 'party'
  ($rootScope, $scope, $location, $filter, display, constants, models, messages, party) ->
    display.title = party.name
    $scope.party = party

    class Item
      class: () ->
        switch s = @selected
          when true then ["radio-selected"]
          when undefined then (if $scope.selected then [] else ["radio"])
          else ["user-access", constants.permission[s.individual]]

      Object.defineProperty @prototype, 'selected',
        get: -> @constructor.selection[@id]

      select: () ->
        s = @selected
        @constructor.selection = {}
        messages.clear(Item)
        if s == true
          $scope.selected = undefined
          $scope.editable = undefined
          @constructor.foreign.selection = {}
        else
          $scope.selected = @
          $scope.editable = @volume?.checkPermission(constants.permission.ADMIN)
          @constructor.selection[@id] = true
          @constructor.foreign.selection = @access
          messages.add
            body: "Highlighting " + @selectionMessage()
            type: 'dark'
            owner: Item
          @selectionMessage()
        return

    class Party extends Item
      @all = {}

      constructor: (@party) ->
        @access = {}
        Party.all[@party.id] = @

      @make = (p) ->
        Party.all[p.id] || new Party(p)

      Object.defineProperty @prototype, 'id',
        get: -> @party.id

      @selection = {}

      selectionMessage: ->
        constants.message('profile.parties.selected', @party.name)

      edit: (t) ->
        if $scope.editable
          $scope.selected.editAccess(@party)
        else if t
          $location.url(party.editRoute(t)+'#auth-'+@party.id)

    class Volume extends Item
      constructor: (@volume) ->
        @access = {}
        for a in @volume.access
          p = Party.make(a.party)
          p.access[@volume.id] = a
          @access[p.party.id] = a

      Object.defineProperty @prototype, 'id',
        get: -> @volume.id
      Object.defineProperty @prototype, 'self',
        get: -> @access[party.id]

      @selection = {}

      selectionMessage: ->
        constants.message('profile.volumes.selected', @volume.displayName)

      editAccess: (p) ->
        $location.url(@volume.editRoute('access'))
        if p
          remove = $rootScope.$on 'volumeEditAccessForm-init', (event, form) ->
            form.preSelect(p)
            remove()

      edit: () ->
        if (s = $scope.selected) && (s == @ || p = s.party) && @volume.checkPermission(constants.permission.ADMIN)
          @editAccess(p)
        else if @volume.checkPermission(constants.permission.EDIT)
          $location.url(@volume.editRoute())

    Party.foreign = Volume
    Volume.foreign = Party

    volumes =
      individual: [] # volumes with individual >= ADMIN
      collaborator: [] # volumes with individual < ADMIN
      inherited: {} # volumes with parent.children > NONE, by parent
    $scope.parties = parties =
      parents: for a in party.parents
        p = Party.make(a.party)
        p.parent = a
        volumes.inherited[p.party.id] = [] if a.member
        p
      children: [] # children indexed by .member access
      collaborators: [] # all other parties on volumes with permission >= ADMIN
    collaborators = {}

    for v in party.volumes
      v = new Volume(v)
      if s = v.self # current party has some permission
        (if s.individual >= constants.permission.ADMIN then volumes.individual else volumes.collaborator).push(v)
      else
        # add to inherited list for each parent with children > NONE
        for ii, il of volumes.inherited
          if v.access[ii]?.children
            il.push(v)
      if v.volume.permission >= constants.permission.ADMIN
        # add everyone on volume to collaborators if permission >= ADMIN
        for pi of v.access
          collaborators[pi] = true

    # cull collaborators accounted for elsewhere
    delete collaborators[party.id]
    for a in party.parents
      delete collaborators[a.party.id]
    for a in party.children
      delete collaborators[a.party.id]
      p = Party.make(a.party)
      p.child = a
      (parties.children[a.member] || (parties.children[a.member] = [])).push(p)
    parties.collaborators = (Party.all[pi] for pi of collaborators)

    stringSort = (a,b) -> +(a > b) || +(a == b) - 1
    volumeSort = (a,b) -> b.volume.permission - a.volume.permission || stringSort(a.volume.displayName, b.volume.displayName)
    partySort = (a,b) -> stringSort(a.party.sortname, b.party.sortname)

    volumes.individual.sort(volumeSort)
    volumes.individual.type = "individual"
    volumes.collaborator.sort(volumeSort)
    volumes.collaborator.type = "collaborator"
    $scope.volumes = [volumes.individual, volumes.collaborator]
    for ii, il of volumes.inherited
      p = Party.all[ii]
      il.sort(volumeSort)
      il.type = "inherited"
      il.parent = Party.all[ii]
      $scope.volumes.push(il)

    parties.parents.sort(partySort) # .parent.site/member?
    parties.parents.type = "parents"
    for cl in parties.children when cl
      cl.sort(partySort)
    parties.collaborators.sort(partySort)
    parties.collaborators.type = 'collaborators'

    today = Date.now()
    $scope.isExpired = (a) ->
      new Date(a.expires) < today

    return
]
