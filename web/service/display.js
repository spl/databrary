'use strict';

app.factory('displayService', [
  '$rootScope', 'storageService', '$filter', 'messageService', 'tooltipService', 'constantService', '$timeout', '$window', 'analyticService',
  function ($rootScope, storage, $filter, messages, tooltips, constants, $timeout, window, analytics) {
    var display = {};

    display.title = '';

    display.loading = false;

    $rootScope.$on('$routeChangeStart', function () {
      display.loading = true;
      display.error = false;
      tooltips.clear();
      messages.clear();
    });

    $rootScope.$on('$routeChangeSuccess', function () {
      display.loading = false;
      analytics.add('open');
    });

    display.error = undefined;

    $rootScope.$on('$routeChangeError', function (event, current, previous, error) {
      display.error = true;
      display.loading = false;
      display.scrollTo(0);
      $rootScope.$broadcast('displayService-error', error);

      var data = {current: current.controller, error: error};
      if (previous)
        data.previous = previous.controller;
      analytics.add('close', data);
    });

    var $scroll = $('html,body');

    display.scrollTo = function (target) {
      $timeout(function () {
        if (angular.isFunction(target))
          target = target();
        if (angular.isString(target))
          target = $(target);
        if (!angular.isNumber(target)) {
          if (!target.length) return;
          target = target.offset().top - 72;
        }
        $scroll.animate({
          scrollTop: target
        }, 500);
      }, 1);
    };

    var ageKeys = ['auto', 'day', 'month', 'year'];
    display.age = storage.get('displayAge') || 'auto';

    display.toggleAge = function () {
      display.age = ageKeys[(ageKeys.indexOf(display.age) + 1) % ageKeys.length];
      $rootScope.$broadcast('displayService-toggleAge', display.age);
      storage.set('displayAge', display.age);
    };

    display.formatAge = function (value) {
      return $filter('age')(value);
    };

    /*$routeChangeStart is always fires before $locationChangeStart*/
    display.cancelRouteChange = function(event){
      display.loading = false;
      event.preventDefault();
    };

    /* TODO: this should really use .canPlayType */
    if (window.navigator.userAgent.search(/^Mozilla\/.* \(Macintosh; .* Firefox\/([0-2]|[3][0-4])/) === 0)
      messages.add({
        type: 'yellow',
        body: constants.message('video.unsupported'),
        persist: true
      });

    return display;
  }
]);
