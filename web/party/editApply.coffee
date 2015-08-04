'use strict'

app.directive 'partyEditApplyForm', [
  'constantService', 'messageService', 'displayService',
  (constants, messages, display) ->
    restrict: 'E'
    templateUrl: 'party/editApply.html'
    link: ($scope) ->
      form = $scope.partyEditApplyForm
      form.data = $scope.party.parents.slice()

      $scope.authSearchSelectFn = (found) ->
        form.data.push
          new: true
          party: found
        display.scrollTo('fieldset article.permission-auth.pef:last')
        return

      $scope.authSearchNotFoundFn = (query) ->
        form.data.push
          new: true
          query: query
        display.scrollTo('fieldset article.permission-auth.pef:last')
        return

      $scope.authApplySuccessFn = (auth) ->
        messages.add
          body: constants.message('auth.apply.save.success')
          type: 'green'
          owner: form

        form.data.remove(auth) unless auth.party
        return

      $scope.authApplyCancelFn = (auth) ->
        messages.add
          body: constants.message('auth.apply.remove.success')
          type: 'green'
          owner: form

        form.data.remove(auth)
        return

      form.preSelect = (p) ->
        if form.data.some((auth) -> auth.party.id == p.id)
          display.scrollTo("#auth-"+p.id)
        else
          $scope.authSearchSelectFn(p)
        return

      $scope.$emit('partyEditApplyForm-init', form)
      return
]
