'use strict';

app.directive('partyEditApplyForm', [
  'pageService', function (page) {
    var link = function ($scope) {
      var party = $scope.party;
      var form = $scope.partyEditApplyForm;

      form.data = party.parents.slice();

      $scope.$on('authApplyForm-init', function (event, applyForm) {
        applyForm.successFn = function (auth) {
          page.messages.add({
            body: page.constants.message('auth.apply.save.success'),
            type: 'green',
            owner: form
          });

          if (!auth.party)
            form.data.remove(auth);
        };

        applyForm.cancelFn = function (auth) {
          page.messages.add({
            body: page.constants.message('auth.apply.remove.success'),
            type: 'green',
            owner: form
          });

          form.data.remove(auth);
        };

        event.stopPropagation();
      });

      var preSelect;
      form.preSelect = function (party) {
        preSelect = party;
      };

      $scope.$on('authSearchForm-init', function (event, searchForm) {
        if (searchForm.principal === 'child')
          return;

        searchForm.selectFn = function (found) {
          form.data.push({
            new: true,
            party: found,
          });
          //warning: next line is template dependent! if classnames change this will no longer work
          page.display.scrollTo('fieldset article.permission-auth.pef:last');
        };

        searchForm.notFoundFn = function (query) {
          form.data.push({
            new: true,
            query: query
          });
          //warning: next line is template dependent! if classnames change this will no longer work
          page.display.scrollTo('fieldset article.permission-auth.pef:last');
        };

        if (preSelect) {
          if (form.data.some(function (auth) {
              return auth.party.id == preSelect.id;
            }))
            page.display.scrollTo("#auth-"+preSelect.id);
          else
            searchForm.selectFn(preSelect);
        }
        preSelect = null;
      });

      $scope.$emit('partyEditApplyForm-init', form);
    };

    return {
      restrict: 'E',
      templateUrl: 'party/editApply.html',
      link: link
    };
  }
]);
