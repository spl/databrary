'use strict';

app.directive('loginForm', [
  'modelService', 'routerService', '$timeout',
  function (models, router, $timeout) { return {
    restrict: 'E',
    templateUrl: 'party/loginForm.html',
    link: function ($scope) {
      var form = $scope.loginForm;

      form.data = {};

      form.submit = function () {
        form.$setSubmitted();
        models.Login.login(form.data).then(function () {
          form.validator.server({});
          form.$setPristine();
          router.back();
        }, function (res) {
          form.$setUnsubmitted();
          form.validator.server(res, true);
        });
      };

      form.validator.client({}, true);

      $timeout(function(){angular.element('#loginEmail').focus();},300);
    }
  }; }
]);
