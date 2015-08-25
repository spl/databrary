'use strict';

app.factory('tooltipService', [
  '$rootScope', '$timeout', '$document',
  function ($rootScope, $timeout, $doc) {

    var defaults = {
      cls: 'tooltip-blue',
      style: {},
      live: false,
      delay: 500,
    };

    var sequence = 0;

    function Tooltip(init) {
      this.id = init.id || 'tooltip-' + sequence++;
      angular.extend(this, defaults, init);
      Tooltip.list[this.id] = this;
      target(this);
    }

    Tooltip.list = {};

    Tooltip.prototype.remove = function () {
      removeEvents(this);
      delete Tooltip.list[this.id];
    };

    var focusElements = ['INPUT', 'SELECT', 'TEXTAREA'];
    function getTargetEvents(tooltip) {
      if (!tooltip.$target) {
        return [];
      }

      var namespace = '.tooltipTarget-' + tooltip.id;

      if (!angular.isString(tooltip.$target) && focusElements.indexOf(tooltip.$target.prop('tagName')) >= 0) {
        return [
            'focusin' + namespace,
            'focusout' + namespace
        ];
      }

      return [
          'mouseenter' + namespace,
          'mouseleave' + namespace
      ];
    }

    function removeEvents(tooltip) {
      if (tooltip.$target) {
        if (tooltip.live) {
          $doc.off(getTargetEvents(tooltip).join(' '), tooltip.$target);
        }
        else {
          tooltip.$target.unbind(getTargetEvents(tooltip).join(' '));
        }
      }
    }

    function target(tooltip) {
      removeEvents(tooltip);

      if (!(tooltip.id in Tooltip.list))
        return;

      var $target = tooltip.$target;

      if (!tooltip.live && $target.length === 0)
        return tooltip.remove();

      var events = getTargetEvents(tooltip);

      var timeout;

      if (tooltip.live) {
        $doc.on(events[0], $target, function (event) {
          var target = $(event.target);
          timeout = $timeout(function () {
            if (document.body.contains(target[0]) && target.is($target)) // may have changed
              tooltip.target = event.target;
          }, tooltip.delay);
        });

        $doc.on(events[1], $target, $rootScope.$lift(function () {
          tooltip.target = undefined;
          $timeout.cancel(timeout);
        }));
      } else {
        $target.bind(events[0], function (event) {
          timeout = $timeout(function () {
            tooltip.target = event.target;
          }, tooltip.delay);
        });

        $target.bind(events[1], $rootScope.$lift(function () {
          tooltip.target = undefined;
          $timeout.cancel(timeout);
        }));
      }

      tooltip.target = undefined;

      return tooltip;
    }

    //

    $rootScope.$watch(function () {

      _.each(Tooltip.list, function(tooltip){
        if (!angular.isString(tooltip.$target) &&
           !document.body.contains(tooltip.$target[0]))
           tooltip.remove();
      });
    });

    Tooltip.clear = function () {
       _.each(Tooltip.list, function (tooltip) {
        tooltip.target = undefined;
      });
    };

    return Tooltip;
  }
]);
