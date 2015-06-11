'use strict';

app.directive('authApplyForm', [
  'pageService', function (page) {
    var link = function ($scope) {
      var party = $scope.party || page.models.Login.user;
      var auth = $scope.auth;
      var form = $scope.authApplyForm;

      form.data = {};

      if (auth.new)
        form.$setDirty();

      //

      var saveAuth = function () {
        form.$setSubmitted();
        party.authorizeApply(auth.party.id, form.data).then(function () {
          form.$setUnsubmitted();
          form.validator.server({});
          form.$setPristine();
          delete auth.new;

          form.successFn();
        }, function (res) {
          form.validator.server(res);
          page.display.scrollTo(form.$element);
        });
      };

      var saveQuery = function () {
        page.messages.clear(form);
        party.authorizeNotFound(angular.extend({
          name: auth.query
        }, form.data)).then(function () {
          form.validator.server({});
          form.$setPristine();
          delete auth.new;

          page.messages.add({
            type: 'green',
            body: page.constants.message('auth.request.notfound.success'),
            owner: form
          });

          form.successFn();
        }, function (res) {
          form.validator.server(res);
          page.display.scrollTo(form.$element);
        });
      };

      form.save = function () {
        if (auth.party)
          saveAuth();
        else
          saveQuery();
      };

      //

      form.cancel = function () {
        form.cancelFn(auth);
      };

      //

      form.validator.client({}, true);

      //

      $scope.$emit('authApplyForm-init', form);
    };

    //

    return {
      restrict: 'E',
      templateUrl: 'party/authApply.html',
      scope: false,
      replace: true,
      link: link
    };
  }
]);
