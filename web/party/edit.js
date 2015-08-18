'use strict';

app.controller('party/edit', [
  '$scope', 'party', 'pageService', function ($scope, party, page) {
    $scope.party = party;
    page.display.title = page.constants.message('party.edit');

    function leavingSoSoon() {
      return $scope.partyEditForm.resetAll(false, true);
    }

    $scope.switchStep = leavingSoSoon;

    $scope.$on('$locationChangeStart', function (event, url) {
      /* hacky: */
      if (url.includes(party.editRoute()))
        return;
      if (!leavingSoSoon())
        return page.display.cancelRouteChange(event);
    });
  }
]);
