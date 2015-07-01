'use strict';

app.directive('volumeList', [
  'pageService',
  function (page) { return {
    restrict: 'E',
    scope: {
      volumes: "=data",
    },
    templateUrl: 'volume/list.html',
    link: function ($scope) {
      $scope.page = page;
      $scope.profile = page.$location.path() === '/profile';
      $scope.name = function (volume) {
        return $scope.profile && volume.alias || volume.name;
      };
      $scope.accessList = function (volume,min,max){
        return volume.access.filter(function(a) {
          return a.individual >= min && a.individual <= max;
        });
      };
    }
  }; }
]);

