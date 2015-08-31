'use strict';

app.directive('loginForm', [
  '$route', 'modelService', 'routerService', '$timeout',
  function ($route, models, router, $timeout) { return {
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
          if ($route.current.controller === 'party/login')
            router.back();
          else
            $route.reload();
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
