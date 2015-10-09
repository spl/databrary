'use strict';

app.controller('party/login', [
  '$location', 'displayService', 'constantService', 'modelService',
  function ($location, display, constants, models) {
    display.title = "Login";
    if (models.Login.isLoggedIn())
      $location.url('/');
  }
]);
