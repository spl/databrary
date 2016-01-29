'use strict'

app.directive 'volumeAssist', [
  'constantService', 'uploadService', 'messageService',
  (constants, uploads, messages) ->
    restrict: 'E'
    templateUrl: 'volume/assist.html'
    link: ($scope) ->
      volume = $scope.volume
      form = $scope.volumeAssist

      $scope.flowOptions = uploads.flowOptions()

      staffPerm = constants.permission.EDIT
      updateStaff = () ->
        staff = volume.access.find((a) -> a.party.id == constants.party.STAFF)?.children
        if !staff
          $scope.staff = false
        else if staff == staffPerm
          $scope.staff = true
        form.staff = $scope.staff
        return
      updateStaff()

      $scope.setStaff = (set) ->
        p = if set then staffPerm else 0
        form.$setSubmitted()
        volume.accessSave(constants.party.STAFF, if set then staffPerm else 0).then () ->
              form.$setPristine()
              updateStaff()
              return
          , (res) ->
              form.$setUnsubmitted()
              messages.addError
                body: constants.message('access.preset.save.error')
                report: res
                owner: form
              return

      $scope.placeholderFiles = [1,2,3]
      return
]
