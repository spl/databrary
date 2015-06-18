 'use strict';

app.controller('party/profile', [
  '$scope', 'displayService', 'party', 'pageService',
  function ($scope, display, party, page) {

    // This function takes in a user, and loops through
    // volumes, and attaches a "volumeSelected" class 
    // to all the pertinent volumes.  
    $scope.clickUser = function(user) {
      for(var i = 0; i < $scope.volumes; i += 1){
        for(var j = 0; j < $scope.volumes[i].users.length; j += 1 ){
          if($scope.volumes[i].users[j].id === user.id){
            $scope.volumes[i].selectedClass = 'volumeSelected'
          } else {
            $scope.volumes[i].selectedClass = ''
          }
        }
      }
    };

    // This function takes in a volume, and loops
    // through the users and attaches a class 
    // called "userSelected" 
    $scope.clickVolume = function(volume) {
      for(var i = 0; i < $scope.users; i +=1){
        for(var j = 0; j < $scope.users[i].volumes.length; i += 1){
          if($scope.users[i].volumes[j].id === volume.id){
           $scope.users[i].selectedClass = 'userSelected'
          } else {
           $scope.users[i].selectedClass = ''
          }
        }
      }
    };

    $scope.party = party;
    $scope.volumes = party.volumes;

    console.log("party:", $scope.party);
    console.log("volume", $scope.volumes);

    $scope.page = page;
    $scope.profile = page.$location.path() === '/profile';
    display.title = party.name;
  }
]);
