 'use strict';

app.controller('party/profile', [
  '$scope', 'displayService', 'party', 'pageService',
  function ($scope, display, party, page) {

    var getUsers = function(volumes){
      var tempUsers = _(volumes).pluck('access').flatten().value();
      var users = {
        sponsors: _.filter(tempUsers, function(u){
          //placeholder value. 
          return u.party.institution === true;
        }),
        owners: _.filter(tempUsers, function(u){
          //placeholder value
          return u.party.permission === 5;
        })
      };
       
      return users;
    };
    
    $scope.clickVolume = function(volume) {
      for(var i = 0; i < $scope.users.sponsors.length; i += 1){
        $scope.users.sponsors[i].isSelected = "";
        for(var j = 0; j < volume.access.length; j += 1){
          if($scope.users.sponsors[i].id === volume.access[j].party.id){
            $scope.users.sponsors[i].isSelected = "usersSelected";
          }
        }
      }
    };

    // This should take in a user, then select volumes on each thing. 
    $scope.clickUser = function(user){
      for(var i = 0; i < $scope.volumes.length; i += 1) {
        $scope.volumes[i].isSelected = "";
        for(var j = 0; j < $scope.volumes[i].access.length; j += 1){
          if($scope.volumes[i].access[j].id == user.party.id){
            $scope.volumes[i].isSelected = "volumeSelected";
          }
        }
      }
    };

    $scope.party = party;
    $scope.volumes = party.volumes;
    $scope.users = getUsers($scope.volumes);  

    $scope.page = page;
    $scope.profile = page.$location.path() === '/profile';
    display.title = party.name;
  }
]);
