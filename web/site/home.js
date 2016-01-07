'use strict';

app.controller('site/home', [
  '$scope', 'constantService', 'displayService', 'volume', 'tags', 'activity',
  function ($scope, constants, display, volume, tags, activity) {
    display.title = constants.message('welcome.title');
    $scope.volume = volume;
    $scope.tags = tags;
    $scope.activity = activity.activity;
    $scope.stats = activity.stats;
  }
]);
