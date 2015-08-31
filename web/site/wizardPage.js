'use strict';

app.directive('wizardPage', [
  function () {
    return {
      restrict: 'E',
      templateUrl: 'site/wizardPage.html',
      require: '^wizard',
      scope: {},
      transclude: true,
      link: {
        pre: function ($scope, $element, $attrs, wizard) {
          $scope.name = $attrs.name;
          $scope.id = wizard.name + '-' + $scope.name;
        },
        post: function ($scope, $element, $attrs, wizard) {
          $scope.complete = true;
          wizard.addStep($scope);
        }
      }
    };
  }
]);

