'use strict';

app.filter('age', [
  'constantService', 'displayService',
  function (constants, display) {
    var AGE = constants.age;
    return function (days, outputFormat) {
      if (!isFinite(days))
        return days;

      if (days >= AGE.limit)
        return "90+ yrs";

      switch (outputFormat || display.age) {
        case 'year':
          return (days/AGE.year).toFixed(1) + ' yrs';

        case 'month':
          return (days/AGE.month).toFixed(1) + ' mos';

        case 'day':
          return days.toFixed() + ' days';

        default:
          var months = days/AGE.month;
          if (months < 3)
            return days.toFixed() + ' days';
          else if (months < 37)
            return months.toFixed(1) + ' mos';
          else
            return (days/AGE.year).toFixed(1) + ' yrs';
      }
    };
  }
]);
