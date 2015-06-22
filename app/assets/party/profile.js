 'use strict';

app.controller('party/profile', [
  '$scope', 'displayService', 'party', 'pageService','constantService',
  function ($scope, display, party, page, constants) {

    var getUsers = function(volumes){

      var users = {};
      
      users.sponsors = [];
      users.labGroupMembers = [];
      users.nonGroupAffiliates = [];
      users.otherCollaborators = [];

      _(volumes).pluck('access').flatten().each(function(v){
        if(v.children > 0 ){
          users.sponsors.push(v);
        } else if(v.parents && v.parents.length > 0 && v.party.members && v.party.members.length > 0) {
          users.labGroupMembers.push(v);
        } else if(v.parents && v.parents.length > 0){
         users.nonGroupAffiliates.push(v);  
        } else {
          users.otherCollaborators.push(v);
        }

      }).value();

      return users;
    };
    
    $scope.clickVolume = function(volume) {
      $scope.volumes = _.map($scope.volumes, function(v){
        v.isSelected = '';
        return v; 
      });

      for(var i = 0; i < $scope.users.sponsors.length; i += 1){
        $scope.users.sponsors[i].isSelected = '';
        for(var j = 0; j < volume.access.length; j += 1){
          if($scope.users.sponsors[i].id === volume.access[j].party.id){
            $scope.users.sponsors[i].isSelected = 'userSelected';
          }
        }
      }
    };

    // This should take in a user, then select volumes on each thing. 
    $scope.clickUser = function(user){
      for(var i = 0; i < $scope.volumes.length; i += 1) {
        $scope.volumes[i].isSelected = '';
        for(var j = 0; j < $scope.volumes[i].access.length; j += 1){
          if($scope.volumes[i].access[j].id == user.party.id){
            $scope.volumes[i].isSelected = 'volumeSelected';
          }
        }
      }
    };

    var getVolumes = function(v) {

      return v;
    };

    $scope.party = party;
    $scope.volumes = party.volumes;
    $scope.users = getUsers($scope.volumes);  
    console.log($scope.users);
    $scope.page = page;
    $scope.profile = page.$location.path() === '/profile';
    display.title = party.name;
  }
]);
