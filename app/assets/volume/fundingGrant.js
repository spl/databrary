'use strict';

app.directive('fundingGrantForm', [
  'pageService', function (page) {
    var link = function ($scope) {
      var volume = $scope.volume;
      var funding = $scope.funding;
      var form = $scope.fundingGrantForm;

      form.data = {
        awards: funding.awards.slice(0)
      };
      form.data.awards.push('');

      if (funding.new)
        form.$setDirty();

      function keep(x) {
        return x;
      }

      form.save = function () {
        page.messages.clear(form);
        form.data.awards = _.filter(form.data.awards, keep);

        form.$setSubmitted();
        volume.fundingSave(funding.funder.id, form.data).then(function () {
          page.messages.add({
            body: page.constants.message('funding.save.success'),
            type: 'green',
            owner: form
          });

          delete funding.new;
          form.data.awards.push('');
          form.$setPristine();
          form.$setUnsubmitted();
        }, function (res) {
          form.$setUnsubmitted();
          page.messages.addError({
            body: page.constants.message('funding.save.error'),
            report: res,
            owner: form
          });

          page.display.scrollTo(form.$element);
        });
      };

      form.remove = function () {
        page.messages.clear(form);
        form.$setSubmitted();
        volume.fundingRemove(funding.funder.id).then(function () {
          page.messages.add({
            body: page.constants.message('funding.remove.success'),
            type: 'green',
            owner: form
          });

          form.$setPristine();
          form.$setUnsubmitted();
          form.removeSuccessFn(funding);
        }, function (res) {
          form.$setUnsubmitted();
          page.messages.addError({
            body: page.constants.message('funding.remove.error'),
            report: res,
            owner: form
          });

          page.display.scrollTo(form.$element);
        });
      };

      form.awardChange = function () {
        if (form.data.awards[form.data.awards.length-1] !== '')
          form.data.awards.push('');
      };

      form.awardRemove = function (i) {
        form.data.awards[i] = i === form.data.awards.length-1 ? '' : null;
        if (i === 0 && !form.data.awards.some(keep))
          return form.remove();
        form.awardChange();
        form.$setDirty();
      };

      $scope.$emit('fundingGrantForm-init', form, $scope);
    };

    //

    return {
      restrict: 'E',
      templateUrl: 'volume/fundingGrant.html',
      link: link
    };
  }
]);
