'use strict';

app.factory('constantService', [
  '$log', '$sce', 'constantData', 'messageData',
  function ($log, $sce, constants, messages) {

    constants.messages = messages;

    constants.regex = {
      doi: /^(?:[dD][oO][iI]:|[hH][dD][lL]:|(?:http:\/\/)?dx\.doi\.org\/)?(10\.[0-9\.]+\/\S+)\s*$/,
    };

    //

    function invertArray(data) {
      for (var i = 0; i < data.length; i ++)
        data[data[i]] = i;
    }

    constants.release['undefined'] = 'DEFAULT';
    constants.releases = Object.keys(constants.release);
    invertArray(constants.permission);
    invertArray(constants.release);
    constants.categoryName = _.indexBy(constants.category, 'name');
    constants.metricName = _.indexBy(constants.metric, 'name');

    /* convenient aliases: */
    constants.permission.VIEW = constants.permission.PUBLIC;
    constants.permission.CONTRIBUTE = constants.permission.EDIT;
    constants.permission.SUPER = constants.permission.length;

    /* backwards compatibility: */
    _.forEach(constants.party, function (party, name) {
      var uname = name.toUpperCase();
      if (angular.isObject(party) && name !== uname)
        constants.party[uname] = party.id;
    });

    _.forEach(constants.category, function (cat) {
      var m = 'not.' + cat.name;
      cat.not = m in messages ? messages[m] : 'No ' + cat.name;
    });

    _.forEach(constants.format, function (fmt) {
      var m = fmt.mimetype;
      fmt.type = m.slice(0, m.indexOf('/'));
    });

    constants.accessPreset = [
      [constants.permission.NONE,   constants.permission.NONE],
      [constants.permission.PUBLIC, constants.permission.SHARED]
    ];
    constants.accessPreset.parties = [
      constants.party.NOBODY,
      constants.party.ROOT
    ];

    constants.message = function (key /*, args...*/) {
      var msg = messages[key];

      if (msg === undefined) {
        $log.info('Message key [' + key + '] is undefined.');
        return '[' + key + ']';
      }

      var i = 1, l = arguments.length;
      var o;
      if (i < l && typeof arguments[i] === 'object')
        o = arguments[i++];
      else
        o = {};

      var g = o.sce ? $sce.getTrusted.bind($sce, o.sce) : angular.identity;
      for (var n = 0; i < l; i++, n++)
        msg = msg.replace('{' + n + '}', g(arguments[i]), 'g');
      if (o.sce)
        msg = $sce.trustAs(o.sce, msg);

      return msg;
    };

    function deepFreeze(o) {
      Object.freeze(o);
      for (var f in o)
        if (o.hasOwnProperty(f)) {
          var v = o[f];
          if (v instanceof Object && !Object.isFrozen(v))
            deepFreeze(v);
        }
      return o;
    }
    constants.deepFreeze = deepFreeze;

    return deepFreeze(constants);
  }
]);
