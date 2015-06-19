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
            $scope.volumes[i].selectedClass = 'volumeSelected';
          } else {
            $scope.volumes[i].selectedClass = '';
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
           $scope.users[i].selectedClass = 'userSelected';
          } else {
           $scope.users[i].selectedClass = '';
          }
        }
      }
    };

    // This is a basic helper function to extract out the 
    // users from the volumes.  The basic logic is 
    // "take a list of volumes in, extract out the 
    // users, then flatten. "
    var getUsers = function(volumes){
      var users = {};
      var tempUsers = _(volumes).pluck('access').flatten().value();
      console.log(tempUsers);
      users.sponsors = _.filter(tempUsers, function(u){
        //Placeholder value. 
        return u.party.institution === true;
      });

      users.owners = _.filter(tempUsers, function(u){
        //placeholder value
        return u.party.permission === 5;
      });
       
      return users;
    };

    $scope.party = party;
    $scope.volumes = party.volumes;
    $scope.users = getUsers($scope.volumes);  

    console.log("party:", $scope.party);
    console.log("volume", $scope.volumes);
    console.log("users: ", $scope.users);
    $scope.page = page;
    $scope.profile = page.$location.path() === '/profile';
    display.title = party.name;
  }
]);
