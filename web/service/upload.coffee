'use strict'

app.factory('uploadService', [
  'routerService',
  (router) ->
    removedAsset: undefined

    flowOptions: () ->
      target: router.controllers.uploadChunk.route()
      method: 'octet'
      chunkSize: 4194304
      simultaneousUploads: 1
      testChunks: false
      chunkRetryInterval: 5000
      permanentErrors: [400, 403, 404, 409, 415, 500, 501]
      successStatuses: [200, 201, 202, 204],
      progressCallbacksInterval: 500
      prioritizeFirstAndLastChunk: true
      headers: {'x-csverf': router.http.csverf}
])
