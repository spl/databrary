'use strict'

app.directive 'volumeAssist', [
  'constantService', 'uploadService', 'messageService', 'routerService',
  (constants, uploads, messages, router) ->
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

      slot = volume.top
      $scope.assets = slot.assets
      $scope.uploads = []

      $scope.fileAdded = (file) ->
        $scope.uploads.push(file)
        router.http(router.controllers.uploadStart, volume.id,
            filename: file.name
            size: file.size
          ).then (res) ->
            file.uniqueIdentifier = res.data
            file.progressValue = 0
            file.resume()
            return
          , (res) ->
            messages.addError
              type: 'red'
              body: constants.message('asset.upload.rejected', {sce:$sce.HTML}, file.name)
              report: res
              owner: this
            file.cancel()
            $scope.uploads.remove(file)
            false
        return

      $scope.fileSuccess = (file) ->
        file.progressValue = 1
        slot.createAsset(
            upload: file.uniqueIdentifier
            name: file.name
            position: null
            classification: null
          ).then (asset) ->
              file.cancel()
              $scope.uploads.remove(file)
              return
            , (res) ->
              messages.addError
                type: 'red'
                body: constants.message('asset.update.error', file.name)
                report: res
                owner: this
              file.cancel()
              file.progressValue = null
              return
        $scope.uploads.remove(file)

      $scope.fileProgress = (file) ->
        file.progressValue = file.progress()
      $scope.fileError = (file, message) ->
        messages.addError
          type: 'red'
          body: constants.message('asset.upload.error', file.name, message || 'unknown error')
          owner: this
        file.cancel()
        $scope.uploads.remove(file)
        return
      return
]
