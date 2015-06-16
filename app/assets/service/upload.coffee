'use strict'

app.factory('uploadService', [
  'routerService',
  (router) ->
    removedAsset: undefined

    # callbacks for ng-flow:

    fileSuccess: (file) ->
      file.store.progress = 1
      file.store.save()
      return


    # If there's an error in the file-upload, 
    # let's first kill the upload process, then 
    # send back the error message. 
    fileError: (file, message) ->
      file.store.error message
      return

    fileProgress: (file) ->
      file.store.progress = file.progress()
      return

    flowOptions:
      target: router.controllers.AssetApi.uploadChunk().url
      method: 'octet'
      chunkSize: 4194304
      simultaneousUploads: 3
      testChunks: false
      chunkRetryInterval: 5000
      permanentErrors: [400, 403, 404, 409, 415, 500, 501]
      successStatuses: [200, 201, 202, 204],
      progressCallbacksInterval: 500
      prioritizeFirstAndLastChunk: true
])
