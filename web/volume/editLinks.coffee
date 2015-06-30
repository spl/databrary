'use strict'

app.directive 'volumeEditLinksForm', [
  'pageService',
  (page) ->
    restrict: 'E',
    templateUrl: 'volume/editLinks.html',
    link: ($scope) ->
      volume = $scope.volume
      form = $scope.volumeEditLinksForm

      blank = () ->
        form.data.push
          head: ''
          url: ''

      update = ->
        form.data = _.map volume.links, (ref) ->
          head: ref.head
          url: ref.url
        blank()

      update()

      form.change = () -> blank() unless form.data[form.data.length-1].url == ''

      form.remove = (ref) ->
        ref.removed = true
        ref.head = ''
        ref.url = ''
        form.$setDirty()

      form.save = () ->
        page.messages.clear(form)
        data = _.filter form.data, (ref) -> !ref.removed && (ref.head || ref.url)
        form.$setSubmitted()
        volume.saveLinks(data).then(() ->
            form.validator.server {}
            update()

            page.messages.add
              type: 'green'
              body: page.constants.message('volume.edit.success')
              owner: form

            form.$setPristine()
            form.$setUnsubmitted()
            return
          , (res) ->
            form.$setUnsubmitted()
            form.validator.server res

            page.messages.addError
              body: page.constants.message('volume.edit.error')
              report: res
              owner: form
            return
          )

      return
]
