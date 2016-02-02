'use strict'

app.directive 'volumeAssist', [
  '$sce', 'constantService', 'uploadService', 'messageService', 'routerService',
  ($sce, constants, uploads, messages, router) ->
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
        uploads.upload(volume, file).then (res) ->
            file.progressValue = 0
            return
          , (res) ->
            messages.addError
              type: 'red'
              body: constants.message('asset.upload.rejected', {sce:$sce.HTML}, file.relativePath)
              report: res
              owner: this
            $scope.uploads.remove(file)
            return
        return

      $scope.fileSuccess = (file) ->
        file.progressValue = 1
        slot.createAsset(
            upload: file.uniqueIdentifier
            name: file.relativePath
            position: null
            classification: null
          ).then (asset) ->
              file.cancel()
              $scope.uploads.remove(file)
              return
            , (res) ->
              messages.addError
                type: 'red'
                body: constants.message('asset.update.error', file.relativePath)
                report: res
                owner: this
              file.cancel()
              file.progressValue = null
              return
        $scope.uploads.remove(file)
        return

      $scope.fileProgress = (file) ->
        file.progressValue = file.progress()
        return

      $scope.fileError = (file, message) ->
        messages.addError
          type: 'red'
          body: constants.message('asset.upload.error', file.relativePath, message || 'unknown error')
          owner: this
        file.cancel()
        $scope.uploads.remove(file)
        return

      return
]
