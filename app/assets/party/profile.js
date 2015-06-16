 'use strict';

app.controller('party/profile', [
  '$scope', 'displayService', 'party', 'pageService',
  function ($scope, display, party, page) {

    // This function takes in a user, and loops through
    // volumes, and attaches a "volumeSelected" class 
    // to all the pertinent volumes.  
    $scope.clickUser = function(user) {
      _.each($scope.volumes, function(v, i) {
        $scope.volumes[i].selectedClass = (v.users.indexOf(user.id) > -1) ? "volumeSelected" : "" ;
      });
    };

    // This function takes in a volume, and loops
    // through the users and attaches a class 
    // called "userSelected" 
    $scope.clickVolume = function(volume) {
      _.each($scope.users, function(u, i) {
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
