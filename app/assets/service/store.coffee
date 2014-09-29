'use strict'

module.factory('Store', [
  'constantService', 'routerService', '$filter', 'messageService', 'modelService',
  (constants, router, $filter, messages, models) -> class Store
    constructor: (@slot, asset) ->
      if asset
        @setAsset(asset)
      else
        @fillData()

    setAsset: (@asset) ->
      @fillData()

    fillData: ->
      @data =
        if @asset
          name: @asset.name
          classification: @asset.classification+''
          container: @slot.id # required for position, which has the side-effect of restoring deleted/moved assets
          position: if isFinite(@asset.segment.l) then $filter('timecode')(@asset.segment.l, true)
          excerptOn: @asset.excerpt?
          excerpt: (@asset.excerpt ? 0)+''
        else
          classification: constants.classification.RESTRICTED+''

    Object.defineProperty @prototype, 'name',
      get: ->
        return constants.message('asset.add') unless @file || @asset
        @file && @file.file.name || @asset && @asset.name || constants.message('file')

    remove: ->
      return unless confirm constants.message 'asset.remove.confirm'
      if @file
        @file.cancel()
        delete @file
        return
      @asset.remove().then(=>
          messages.add
            type: 'green'
            countdown: 3000
            body: constants.message('asset.remove.success', @name)
          delete @asset
          true
        , (res) =>
          messages.addError
            type: 'red'
            body: constants.message('asset.remove.error', @name)
            report: res
          false
        )

    save: ->
      (if @file
        @data.upload = @file.uniqueIdentifier
        if @asset then @asset.replace(@data) else @slot.createAsset(@data)
      else
        @asset.save(@data)
      ).then((asset) =>
          if asset instanceof models.Asset
            @asset.asset = asset
            asset = @asset
          @setAsset(asset)

          messages.add
            type: 'green'
            countdown: 3000
            body: constants.message('asset.' + (if @file then 'upload' else 'update') + '.success', @name) +
              (if @file && asset.format.transcodable then ' ' + constants.message('asset.upload.transcoding') else '')

          if @file
            unless 'creation' of asset.asset
              asset.asset.creation = {date: Date.now(), name: @file.file.name}
            @file.cancel()
            delete @file
            delete @progress
          true
        , (res) =>
          messages.addError
            type: 'red'
            body: constants.message('asset.update.error', @name)
            report: res
          if @file
            @file.cancel()
            delete @file
            delete @progress
            delete @data.upload
          false
        )

    upload: (file) ->
      file.pause()
      @file = file
      @progress = 0
      file.store = this
      
      router.http(router.controllers.AssetApi.uploadStart,
          filename: file.name
          size: file.size
        ).then((res) =>
          file.uniqueIdentifier = res.data
          file.resume()
          true
        , (res) =>
          messages.addError
            type: 'red'
            body: constants.message('asset.upload.rejected', @name)
            report: res
          file.cancel()
          delete @file
          delete @progress
          false
        )

    # callbacks for ng-flow:

    @fileSuccess = (file) ->
      file.store.progress = 1
      file.store.save()

    @fileProgress = (file) ->
      file.store.progress = file.progress()

    @flowOptions =
      target: router.controllers.AssetApi.uploadChunk().url
      method: 'octet'
      simultaneousUploads: 3
      testChunks: false
      chunkRetryInterval: 5000
      permanentErrors: [400, 403, 404, 409, 415, 500, 501]
      progressCallbacksInterval: 500
      prioritizeFirstAndLastChunk: true
])