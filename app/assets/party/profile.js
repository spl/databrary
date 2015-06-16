 'use strict';

app.controller('party/profile', [
  '$scope', 'displayService', 'party', 'pageService',
  function ($scope, display, party, page) {
    $scope.clickUser = function(user){
      _.each($scope.volumes, function(v, i){
        $scope.volumes[i].selectedClass = (v.users.indexOf(user.id) > -1) ? "volumeSelected" : "" ;
      });
    };

    $scope.clickVolume = function(volume){
      _.each($scope.users, function(u, i){
        $scope.users[i].selectedClass = (u.volumes.indexOf(volume.id) > -1) ? "userSelected" : "" ;
      });
    };

    $scope.party = party;
    $scope.volumes = party.volumes;
    $scope.page = page;
    $scope.profile = page.$location.path() === '/profile';
    display.title = party.name;
  }
]);
