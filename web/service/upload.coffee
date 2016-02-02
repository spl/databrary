'use strict'

app.factory('uploadService', [
  '$q', 'routerService',
  ($q, router) ->
    removedAsset: undefined

    flowOptions: () ->
      target: router.controllers.uploadChunk.route()
      method: 'octet'
      chunkSize: 4194304
      simultaneousUploads: 3
      testChunks: false
      chunkRetryInterval: 5000
      permanentErrors: [400, 403, 404, 409, 415, 500, 501]
      successStatuses: [200, 201, 202, 204],
      progressCallbacksInterval: 500
      prioritizeFirstAndLastChunk: false
      headers: {'x-csverf': router.http.csverf}

    upload: (volume, file) ->
      file.pause()
      router.http(router.controllers.uploadStart, volume.id,
          filename: file.name
          size: file.size
        ).then (res) ->
          file.uniqueIdentifier = res.data
          file.resume()
          return
        , (res) ->
          file.cancel()
          $q.reject(res)
])
