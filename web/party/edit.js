'use strict';

app.controller('party/edit', [
  '$scope', 'displayService', 'party',
  function ($scope, display, party) {
    $scope.party = party;
    display.title = "Edit " + party.name;

    function leavingSoSoon() {
      return $scope.partyEditForm.resetAll(false, true);
    }

    $scope.switchStep = leavingSoSoon;

    $scope.$on('$locationChangeStart', function (event, url) {
      /* hacky: */
      if (url.includes(party.editRoute()))
        return;
      if (!leavingSoSoon())
        return display.cancelRouteChange(event);
    });
  }
]);
