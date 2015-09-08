'use strict';

app.directive('toolbar', [
  'pageService', function (page) {
    return {
      restrict: 'E',
      templateUrl: 'site/toolbar.html',
      scope: {},
      link: function ($scope) {
        $scope.page = page;
        $scope.hoverUserToggle = function ($event) {
          $scope.hoverUser = !$scope.hoverUser;
          $scope.hoverSearch = false;
          if ($event)
            $event.stopPropagation();
        };
        $scope.hoverSearchToggle = function ($event) {
          $scope.hoverSearch = !$scope.hoverSearch;
          $scope.hoverUser = false;
          if ($event)
            $event.stopPropagation();
        };
        $scope.hoverNavToggle = function ($event) {
          $scope.hoverNav = !$scope.hoverNav;
          $scope.hoverUser = $scope.hoverSearch = false;
          if ($event)
            $event.stopPropagation();
        };
        $scope.$on('$locationChangeStart', function () {
          $scope.hoverUser = false;
          $scope.hoverSearch = false;
          $scope.hoverNav = false;
        });

        $scope.search = function () {
          page.$location.url(page.router.search()).search($scope.search.data);
        };
        $scope.search.data = {};
      }
    };
  }
]);
