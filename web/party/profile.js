'use strict';

app.controller('party/profile', [
  '$scope', 'displayService', 'party', 'pageService','constantService', 'modelService', 
  function ($scope, display, party, page, constants, models) {
    var getUsers = function(volumes){

      var users = {};
      
      users.sponsors = [];
      users.databrary = [];
      users.labOnly = [];
      users.otherCollaborators = [];

      _(volumes).pluck('access').flatten().uniq(function(i){
        return i.party.id;
      }).each(function(v, index, array){

        if(models.Login.user.id === v.party.id){
          v.isCurrent = true;
          users.sponsors.push(v);
        }

        if(v.party.parents && v.party.parents.length){
          users.sponsors = users.sponsors.concat(v.party.parents);
        } else if(v.party.site && v.party.site > page.constants.permission.NONE) {
          users.databrary.push(v);
        } else if(v.party.site == null || v.party.site == page.constants.permission.NONE) {
          users.labOnly.push(v);
        } else {
          users.otherCollaborators.push(v);
        }
      }).value();
      // The "value()" call is to actually force theb chain to work.
      
      var filterOnId = function(i){
        return i.party.id; 
      };

      users.sponsors = _.uniq(users.sponsors, filterOnId);
      users.databrary = _.uniq(users.databrary, filterOnId);
      users.labOnly = _.uniq(users.labOnly, filterOnId);
      users.otherCollaborators = _.uniq(users.otherCollaborators, filterOnId);

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

      return v;
    };

    $scope.unselectAll = function(){

      $scope.volumes.individual = _.map($scope.volumes.individual, unSetSelected);

      $scope.volumes.collaborator = _.map($scope.volumes.collaborator, unSetSelected);

      $scope.volumes.inherited = _.map($scope.volumes.inherited, unSetSelected);

      $scope.users.sponsors = _.map($scope.users.sponsors, unSetSelected);

      $scope.users.nonGroupAffiliates = _.map($scope.users.nonGroupAffiliates, unSetSelected);

      $scope.users.labGroupMembers = _.map($scope.users.labGroupMembers, unSetSelected);

      $scope.users.otherCollaborators = _.map($scope.users.otherCollaborators, unSetSelected);

    }; 

    $scope.clickVolume = function(volume) {

      $scope.unselectAll();
      volume.isSelected = 'volumeSelected';
      volume = _.flatten([volume.v || volume]);
      for(var k = 0; k < volume.length; k++){
        for(var i = 0; i < volume[k].access.length; i++){

          var j;

          for (j = 0; j < $scope.users.sponsors.length; j++){
            if($scope.users.sponsors[j].party.id === volume[k].access[i].party.id){
              $scope.users.sponsors[j].isSelected = 'userSelected';
            }
          }

          for (j = 0; j < $scope.users.labGroupMembers.length; j++) {
            if($scope.users.labGroupMembers[j].party.id === volume[k].access[i].party.id){
              $scope.users.labGroupMembers[j].isSelected = 'userSelected';
            }
          }

          for (j = 0; j < $scope.users.nonGroupAffiliates.length; j++) {
            if($scope.users.nonGroupAffiliates[j].party.id === volume[k].access[i].party.id){
              $scope.users.nonGroupAffiliates[j].isSelected = 'userSelected';
            }
          }

          for (j = 0; j < $scope.users.otherCollaborators.length; j++) {
            if($scope.users.otherCollaborators[j].party.id === volume[k].access[i].party.id){
              $scope.users.otherCollaborators[j].isSelected = 'userSelected';
            }
          }
        }
      }
    };

    // This should take in a user, then select volumes on each thing. 
    $scope.clickUser = function(user){
      $scope.unselectAll();
      user.isSelected = 'userSelected';
      var i, j, k; 

      var compareFunction = function(value, key, array){
        for(j = 0; j < value.access.length; j++){
          if(value.access[j].party.id == user.party.id){
            array[key].isSelected = 'volumeSelected';
          }
        }
      };

      _.each($scope.volumes.individual, compareFunction);
      _.each($scope.volumes.collaborator, compareFunction);

      for(i = 0; i < $scope.volumes.inherited.length; i++){
        for(j = 0; j < $scope.volumes.inherited[i].v.length; j++){
          for(k = 0; k < $scope.volumes.inherited[i].v[j].access.length; k++){
            if($scope.volumes.inherited[i].v[j].access[k].party.id == user.party.id) {
              $scope.volumes.inherited[i].v[j].isSelected = 'volumeSelected';
              $scope.volumes.inherited[i].isSelected = 'volumeSelected';
            }
          }
        }
      }
    };

    var getParents = function(parents) {
      
      return _.compact(_.map(parents, function(p){
        if(p.member) {
          var v = [];
          return {
            p: p,
            v: v
          };
        }
      }));

    };

    var getVolumes = function(volumes) {
      var tempVolumes = {};
      tempVolumes.individual = [];
      tempVolumes.collaborator = [];

      tempVolumes.inherited = getParents(party.parents);

      _.each(volumes, function(v){

        var isCurrent = _.find(v.access, function(r){
          return r.party.id === models.Login.user.id;
        });
        
        var isAdmin = _.find(v.access, function(r) {
          return r.party.authorization === 5; 
        });
        
        if(isCurrent && isAdmin){
          // The "mini-object" with v and [v] is to make sure that the data is all
          // shaped the same, making looping over it *substantially* simpler. 
          tempVolumes.individual.push(v);
        } else if(isCurrent){
          tempVolumes.collaborator.push(v);
        } else {
          for (var i = 0; i < v.access.length; i++) {
            for (var j = 0; j < tempVolumes.inherited.length; j++) {
              if (v.access[i].children && v.access[i].party.id === tempVolumes.inherited[j].p.party.id) {
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
    $scope.users = getUsers(party.volumes);      
    $scope.volumes = getVolumes(party.volumes);

    $scope.page = page;
    $scope.profile = page.$location.path() === '/profile';
    display.title = party.name;

  }
]);
