'use strict';

app.controller('site/home', [
  '$scope', 'constantService', 'displayService', 'volume', 'tags', 'audit',
  function ($scope, constants, display, volume, tags, audit) {
    display.title = constants.message('welcome.title');
    $scope.volume = volume;
    $scope.tags = tags;
    $scope.activity = audit.activity;
    $scope.stats = audit.stats;
  }
]);
