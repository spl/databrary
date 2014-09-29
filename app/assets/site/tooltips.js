'use strict';

module.directive('tooltips', [
  'pageService', function (page) {
    var controller = [function () {
      var tooltips = {};

      tooltips.getTooltipClasses = function (tooltip) {
	var classes = tooltip.cls.split(' ');

	classes.push('tooltip');
	classes.push('tooltip-' + tooltip.type);

	classes.push.apply(classes,
	  tooltip.position.map(function (p) {
	    return 'tooltip-' + p;
	  }));

	if (tooltip.visible)
	  classes.push('tooltip-visible');

	return classes;
      };

      tooltips.list = page.tooltips.list;

      return tooltips;
    }];

    return {
      restrict: 'E',
      replace: true,
      templateUrl: 'site/tooltips.html',
      controller: controller,
      controllerAs: 'tooltips',
    };
  }
]);