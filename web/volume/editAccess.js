'use strict';

app.directive('volumeEditAccessForm', [
  '$q', 'constantService', 'messageService', 'displayService',
  function ($q, constants, messages, display) {
    var link = function ($scope) {
      var volume = $scope.volume;
      var form = $scope.volumeEditAccessForm;

      form.preset = volume.accessPreset;
      form.data = _.values(volume.access);

      var presetForm = $scope.accessPresetForm;
      var subforms = [];

      $scope.permissionName = function (p) {
        return constants.permission[p];
      };

      function savePreset() {
        if (form.preset == null)
          return;
        form.$setSubmitted();
        $q.all(constants.accessPreset[form.preset].map(function (a, pi) {
          volume.accessSave(constants.accessPreset.parties[pi], {
            individual: a,
            children: a,
          });
        })).then(function () {
          form.$setUnsubmitted();
          messages.add({
            body: constants.message('access.preset.save.success'),
            type: 'green',
            owner: form
          });
          form.$setPristine();
        }, function (res) {
          form.$setUnsubmitted();
          messages.addError({
            body: constants.message('access.preset.save.error'),
            report: res,
            owner: form
          });
        });
      }

      form.saveAll = function () {
        messages.clear(form);
        subforms.forEach(function (subform) {
          if (subform.$dirty)
            subform.save(false);
        });
        if (presetForm.$dirty)
          savePreset();
      };

      $scope.$on('accessGrantForm-init', function (event, grantForm) {
        subforms.push(grantForm);

        grantForm.removeSuccessFn = function (access) {
          form.data.remove(access);
          subforms.remove(grantForm);
        };
      });

      $scope.selectFn = function (found) {
        form.data.push({
          new: true,
          party: found,
        });
        //warning: next line is template dependent! if classnames change this will no longer work
        display.scrollTo('fieldset .access-grant:last');
      };
    };

    return {
      restrict: 'E',
      templateUrl: 'volume/editAccess.html',
      link: link
    };
  }
]);
