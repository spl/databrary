'use strict';

app.directive('citeVolume', [
  'constantService', 'routerService', '$location',
  function (constants, router, $location) {
    var link = function ($scope) {
      var authors = '';
      var ai = 0;
      var volume = $scope.volume;
      var access = volume.access && volume.access[ai];

      function initial(p) {
        if (p)
          authors += p.charAt(0) + '.';
      }

      while (access) {
        var next = volume.access[++ai];
        if (next && (next.individual || 0) < constants.permission.ADMIN)
          next = undefined;

        if (authors !== '') {
          authors += ', ';
          if (!next)
            authors += ' & ';
        }

        var parts = access.party.name.split(' ');
        authors += parts.pop();

        if (parts.length) {
          authors += ', ';
          parts.forEach(initial);
        }

        access = next;
      }

      $scope.authors = authors;
      $scope.today = new Date();
      $scope.permalink = (volume.doi ? 'doi:' + volume.doi : $location.absUrl());
    };

    return {
      restrict: 'E',
      templateUrl: 'volume/cite.html',
      scope: false,
      replace: true,
      link: link
    };
  }
]);
