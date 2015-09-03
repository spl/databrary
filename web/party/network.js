'use strict';

app.directive('partyNetwork', [
  '$rootScope', '$sce', '$location', 'constantService', 'messageService', 'modelService',
  function ($rootScope, $sce, $location, constants, messages, models) {
    return {
    restrict: 'E',
    templateUrl: 'party/network.html',
    scope: false,
    link: function ($scope) {
      var actionMessages = {};

      $scope.$on('$destroy', function () {
        _.each(actionMessages, function (bundle) {
          bundle.message.remove();
        });
      });

      $scope.isAdmin = $scope.party.checkPermission(constants.permission.ADMIN);
      if ($scope.isAdmin)
      _.each($scope.party.children, function (party) {
        if (!party.member && !party.site) {
          if (!actionMessages[party.id]) {
            actionMessages[party.id] = {
              party: party,
              message: messages.add({
                type: 'yellow',
                persist: true,
                body: $sce.trustAsHtml('<span>' + constants.message('auth.pending.notice', {sce:$sce.HTML}, party.party.name) + ' <a href="' + $scope.party.editRoute('grant') + '#auth-' + party.party.id + '">Manage</a>.</span>')
              })
            };
          }
          else {
            actionMessages[party.id].party = party;
          }
        }
      });

      //

      var user = models.Login.user.id;
      function isUser(a) {
        return a.party.id === user;
      }

      $scope.isParent =
        $scope.party.parents.some(isUser);
      $scope.isRelation = $scope.isParent ||
        /* you always exist on your own page */
        $scope.party.id <= 0 || $scope.party.id === user ||
        $scope.party.children.some(isUser);

      $scope.grant = function () {
        $location.url(models.Login.user.editRoute('grant'));
        var remove = $rootScope.$on('partyEditGrantForm-init', function (event, form) {
          form.preSelect($scope.party);
          remove();
        });
      };

      $scope.apply = function () {
        $location.url(models.Login.user.editRoute('apply'));
        var remove = $rootScope.$on('partyEditApplyForm-init', function (event, form) {
          form.preSelect($scope.party);
          remove();
        });
      };

      $scope.partyChildrenCount = Object.keys($scope.party.children).length;
      $scope.partyParentsCount = Object.keys($scope.party.parents).length;
    }
    };
  }
]);
