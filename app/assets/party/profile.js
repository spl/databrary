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

      // This is a quick helper function to make sure that the isSelected
      // class is set to empty and to avoid repeating code. 
      var unSetSelected = function(v){
        v.isSelected = '';
        return v; 
      };
      
      $scope.volumes.individual = _.map($scope.volumes.individual, unSetSelected);
      $scope.volumes.collaborator = _.map($scope.volumes.collaborator, unSetSelected);
      $scope.volumes.inherited = _.map($scope.volumes.inherited, unSetSelected);
      
      for(var i = 0; i < $scope.users.sponsors.length; i += 1){
        $scope.users.sponsors[i].isSelected = '';
        $scope.users.labGroupMembers[i].isSelected = '';
        $scope.users.nonGroupAffiliates[i] = '';
        $scope.users.otherCollaborators[i] = ''; 
        for(var j = 0; j < volume.access.length; j += 1){
          if($scope.users.sponsors[i].id === volume.access[j].party.id){
            $scope.users.sponsors[i].isSelected = 'userSelected';
            return; 
          } else if($scope.users.labGroupMembers[i].id === volume.access[j].party.id){
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

    var getVolumes = function(volumes) {
      var tempVolumes = {};
      tempVolumes.individual = [];
      tempVolumes.collaborator = [];
      tempVolumes.inherited = [];

      _.each(volumes, function(v){
        if(v.isIndividual){
          tempVolumes.individual.push(v); 
	} else if(tempVolumes.isCollaborator){
          tempVolumes.collaborator.push(v); 
	} else{
          tempVolumes.inherited.push(v); 
	}
      });
      return tempVolumes;
    };

    $scope.party = party;
    $scope.volumes = getVolumes(party.volumes);
    console.log(party.volumes);
    $scope.users = getUsers(party.volumes);  
    console.log($scope.users);
    $scope.page = page;
    $scope.profile = page.$location.path() === '/profile';
    display.title = party.name;
  }
]);
