'use strict';

app.controller('party/profile', [
  '$scope', 'displayService', 'party', 'pageService','constantService', 'modelService', 
  function ($scope, display, party, page, constants, models) {

    // This function is responsible for extracting out all the users from
    // volumes (which are passed in) and the party (which is not)
    var getUsers = function(volumes){

      // It's easier to handle all the user information by dividing
      // the users into subarrays in a hierarchical user object
      var users = {};

      users.otherCollaborators = [];
      users.sponsors = $scope.party.parents;

      // the databrary users should be every child of the current user that
      // has at least *some* permission on the site. 
      users.databrary = _.filter($scope.party.children, function(i) {
        return i.site > page.constants.permission.NONE;
      });


      // the labOnly users should be children, (like above), that have
      // absolutely no access to the site. 
      users.labOnly = _.filter($scope.party.children, function(i) {
        return i.site === page.constants.permission.NONE;
      });


      // This is a helper variable to grab everything into a single one-dimensional
      // array for comparison later, specifically because "otherCollaborators" should
      // only apply to everything else. 
      var arrayOfEverything = users.sponsors.concat(users.databrary).concat(users.labOnly); 


      
      users.otherCollaborators = _($scope.volumes.individual).pluck('access').flatten().uniq(function(i){
        return i.party.id;
      }).filter(function(i){
        return i.party.id !== models.Login.user.id && (_.findIndex(arrayOfEverything, function(j){
          return i.party.id == j.party.id;
        }) == -1);
      }).value();

      var filterOnId = function(i) {
        return i.party.id; 
      };

      var sorter = function(i){
        return i.party.sortname;
      };

      _.each(users, function(_value, key) {
        users[key] = _(users[key]).uniq(filterOnId).sortBy(sorter).value();
      }); 

      return users;
    };

    $scope.unselectAll = function() {
      
      // This is a quick helper function to make sure that the isSelected
      // class is set to empty and to avoid repeating code. 
      var unSetSelected = function(v) {
        v.isSelected = '';
        if(v.v !== undefined) {

          v.v = _.map(v.v, function(a) {
            a.isSelected = '';
            return a;
          });
        }

        return v;
      };

      $scope.volumes.individual = _.map($scope.volumes.individual, unSetSelected);

      $scope.volumes.collaborator = _.map($scope.volumes.collaborator, unSetSelected);

      $scope.volumes.inherited = _.map($scope.volumes.inherited, unSetSelected);

      $scope.users.sponsors = _.map($scope.users.sponsors, unSetSelected);

      $scope.users.databrary = _.map($scope.users.databrary, unSetSelected);

      $scope.users.labOnly = _.map($scope.users.labOnly, unSetSelected);

      $scope.users.otherCollaborators = _.map($scope.users.otherCollaborators, unSetSelected);

    };

    $scope.clickVolume = function(volume) {

      $scope.unselectAll();
      volume.isSelected = 'volumeSelected';
      volume = _.flatten([volume.v || volume]);


      _.each(volume, function(vol) {
        _.each(vol.access, function(acc) {
          var userSelectFunction = function(user, index, array) {
            if(user.party.id == acc.party.id) {
              array[index].isSelected = 'userSelected';
            }
          };

          _.each($scope.users.sponsors,userSelectFunction);
          _.each($scope.users.databrary,userSelectFunction);
          _.each($scope.users.labOnly,userSelectFunction);
          _.each($scope.users.otherCollaborators,userSelectFunction);
          
        });
      }); 
    };

    // This should take in a user, then select volumes on each thing. 
    $scope.clickUser = function(user) {
      $scope.unselectAll();
      user.isSelected = 'userSelected';
      var i; 

      var compareFunction = function(value, key, array) {
        for(var j = 0; j < value.access.length; j++) {
          if(value.access[j].party.id === user.party.id) {
            array[key].isSelected = 'volumeSelected';
          }
        }
      };

      _.each($scope.volumes.individual, compareFunction);
      _.each($scope.volumes.collaborator, compareFunction);

      for(i = 0; i < $scope.volumes.inherited.length; i++) {
        _.each($scope.volumes.inherited[i].v, compareFunction);
      }
    };

    var getParents = function(parents) {
      return _.compact(_.map(parents, function(p) {
        if(p.member) {
          return _.zipObject(['p', 'v'], [p, []]);
        }
      }));
    };

    var getVolumes = function(volumes) {
      var tempVolumes = _.zipObject(['individual', 'collaborator'], [[], []]); 

      tempVolumes.inherited = getParents(party.parents);

      _.each(volumes, function(v) {

        var isCurrent = _.find(v.access, function(r) {
          return r.party.id === models.Login.user.id;
        });

        if(isCurrent && isCurrent.individual == page.constants.permission.ADMIN) {
          // The "mini-object" with v and [v] is to make sure that the data is all
          // shaped the same, making looping over it *substantially* simpler. 
          tempVolumes.individual.push(v);
        } else if(isCurrent) {
          tempVolumes.collaborator.push(v);
        } else {
          for (var i = 0; i < v.access.length; i++) {
            for (var j = 0; j < tempVolumes.inherited.length; j++) {
              if (v.access[i].children && v.access[i].party.id === tempVolumes.inherited[j].p.party.id) {
                tempVolumes.inherited[j].v.push(v);
              }
            }
          }
        }

        
        
      });

      var getDisplayName = function(i) {
        return i.alias || i.name; 
      };
      
      tempVolumes.individual = _.sortBy(tempVolumes.individual, getDisplayName);
      tempVolumes.collaborator = _.sortBy(tempVolumes.collaborator, getDisplayName);
      
      return tempVolumes;

    };

    $scope.party = party;
    $scope.volumes = getVolumes(party.volumes);
    $scope.users = getUsers(party.volumes);      
    
    console.log("Constants: ",constants);
    console.log($scope.users); 

    $scope.page = page;
    $scope.profile = page.$location.path() === '/profile';
    display.title = party.name;

  }
]);
