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
        console.log("V: ", v); 
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

      console.log(users);
      return users;
    };

    // This is a quick helper function to make sure that the isSelected
    // class is set to empty and to avoid repeating code. 
    var unSetSelected = function(v){
      v.isSelected = '';
      if(v.v !== undefined){
        v.v = _.map(v.v, function(a){
          a.isSelected = '';
          return a; 
        });
      }         

      if(v.p !== undefined){
        v.p = _.map(v.p, function(a){
          a.isSelected = ''; 
          return a; 
        });
      }
      return v; 
    };
      
    var unselectAll = function(){

      $scope.volumes.individual = _.map($scope.volumes.individual, unSetSelected); 
      
      $scope.volumes.collaborator = _.map($scope.volumes.collaborator, unSetSelected);

      $scope.volumes.inherited = _.map($scope.volumes.inherited, unSetSelected);
      
      $scope.users.sponsors = _.map($scope.users.sponsors, unSetSelected);
      $scope.users.nonGroupAffiliates = _.map($scope.users.nonGroupAffiliates, unSetSelected);
      $scope.users.labGroupMembers = _.map($scope.users.labGroupMembers, unSetSelected);
      $scope.users.otherCollaborators = _.map($scope.users.otherCollaborators, unSetSelected);
    }; 
    
    $scope.clickVolume = function(volume) {
      unselectAll();
      volume.isSelected = 'volumeSelected';
        for(var i = 0; i < volume.access.length; i += 1 ){
          var j;
          for (j = 0; j < $scope.users.sponsors.length; j += 1){
            if($scope.users.sponsors[j].party.id === volume.access[i].party.id){
              $scope.users.sponsors[j].isSelected = 'userSelected';
            }
          }
          for (j = 0; j < $scope.users.labGroupMembers.length; j += 1){
            if($scope.users.labGroupMembers[j].party.id === volume.access[i].party.id){
              $scope.users.labGroupMembers[j].isSelected = 'userSelected';
            }
          }
          for (j = 0; j < $scope.users.nonGroupAffiliates.length; j += 1){
            if($scope.users.nonGroupAffiliates[j].party.id === volume.access[i].party.id){
              $scope.users.nonGroupAffiliates[j].isSelected = 'userSelected';
            }
          }
          for (j = 0; j < $scope.users.otherCollaborators.length; j += 1){
            if($scope.users.otherCollaborators[j].party.id === volume.access[i].party.id){
              $scope.users.otherCollaborators[j].isSelected = 'userSelected';
            }
          }        
        
      }
    };

    // This should take in a user, then select volumes on each thing. 
    $scope.clickUser = function(user){
      unselectAll();

      for(var a = 0; a < $scope.volumes.individual.length; a += 1){
        for(var s = 0; s < $scope.volumes.individual[a].v.length; s += 1){
          for(var d = 0; d < $scope.volumes.individual[a].v[s].access.length; d += 1) {
            if($scope.volumes.individual[a].v[s].access[d].party.id == user.party.id){
              $scope.volumes.individual[a].isSelected = 'volumeSelected';
            }
          }
        }
      }

      
      for(var i = 0; i < $scope.volumes.collaborator.length; i += 1){
        for(var j = 0; j < $scope.volumes.collaborator[i].v.length; j += 1){
          for(var k = 0; k < $scope.volumes.collaborator[i].v[j].access.length; k += 1) {
            if($scope.volumes.collaborator[i].v[j].access[k].party.id == user.party.id){
              $scope.volumes.collaborator[i].isSelected = 'volumeSelected';
            }
          }
        }
      }
      
      for(var e = 0; e < $scope.volumes.inherited.length; e += 1){
        for(var r = 0; r < $scope.volumes.inherited[e].v.length; r += 1){
          for(var t = 0; t < $scope.volumes.inherited[e].v[r].access.length; t += 1) {
            if($scope.volumes.inherited[e].v[r].access[t].party.id == user.party.id){
              $scope.volumes.inherited[e].isSelected = 'volumeSelected';
            }
          }
        }
      }
    };

    var getParents = function(parents) {
      var tempParents = [];
      _.each(parents, function(p){
        if(p.member){
          var v = [];
          var tempThing = {
            p: p,
            v: v
          };
          tempParents.push(tempThing);
        }
      });
      return tempParents;
    };

    var getVolumes = function(volumes) {
      var tempVolumes = {};
      tempVolumes.individual = [];
      tempVolumes.collaborator = [];

      tempVolumes.inherited = getParents(party.parents);

      _.each(volumes, function(v){
        if(v.isIndividual){
          tempVolumes.individual.push({v: [v]});
        } else if(tempVolumes.isCollaborator){
          tempVolumes.collaborator.push({v: [v]});
        } else{
          for (var i=0;i<v.access.length;i++){
            for (var j=0;j<tempVolumes.inherited.length;j++){
              if (v.access[i].children && v.access[i].party.id === tempVolumes.inherited[j].p.party.id){
                tempVolumes.inherited[j].v.push(v);
                break;
              }
            }
          }
        }
      });
      return tempVolumes;
    };

    $scope.party = party;
    $scope.volumes = getVolumes(party.volumes);
    console.log("Volumes: ", $scope.volumes); 
    $scope.users = getUsers(party.volumes);  
    $scope.page = page;
    $scope.profile = page.$location.path() === '/profile';
    display.title = party.name;

    console.log($scope.volumes.inherited);
  }
]);
