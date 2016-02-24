'use strict'

app.directive 'partyEditGrantForm', [
  '$q', 'constantService', 'messageService', 'displayService',
  ($q, constants, messages, display) ->
    restrict: 'E',
    templateUrl: 'party/editGrant.html',
    link: ($scope) ->
      form = $scope.partyEditGrantForm
      form.data = $scope.party.children.slice()

      authSearchSelectFn = (found, searchForm) ->
        messages.clear(searchForm) if searchForm
        exp = new Date()
        exp.setFullYear(exp.getFullYear()+2)
        form.data.push
          new: true
          party: found
          site: 0
          member: 0
          expires: exp.getTime()
        display.scrollTo('fieldset article.permission-auth.peg:last')
        return

      $scope.authSearchNotFoundFn = (name, searchForm) ->
        messages.add
          type: 'yellow'
          body: constants.message('auth.grant.notfound')
          owner: searchForm

      $scope.authDenySuccessFn = (auth) ->
        form.data.remove(auth)

      form.saveAll = () ->
        $scope.$broadcast('authGrantSave')

      form.preSelect = (p, searchForm) ->
        if form.data.some((auth) -> auth.party.id == p.id)
          display.scrollTo("#auth-"+p.id)
        else if $scope.party.parents.some((a) -> a.party.id == p.id)
          messages.add
            type: 'red'
            body: constants.message('auth.grant.parent')
            owner: searchForm
        else
          authSearchSelectFn(p, searchForm)
        return

      $scope.authSearchSelectFn = form.preSelect

      $scope.$emit('partyEditGrantForm-init', form)
]
