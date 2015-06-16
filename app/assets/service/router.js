'use strict';

app.provider('routerService', [
  '$routeProvider', 'routeData',
  function ($routeProvider, controllers) {
    function encodeUri(val) {
      return encodeURIComponent(val).
        replace(/%40/g, '@').
        replace(/%3A/gi, ':').
        replace(/%24/g, '$').
        replace(/%2C/gi, ',').
        replace(/%20/g, '+');
    }

    /* Add the query params to the url. */
    function urlParams(url, params) {
      if (!params) {
        return url;
      }

      var q = url.includes('?');

      _.each(params, function (value, key) {
        if (value == null)
          return;

        if (!Array.isArray(value)) {
          value = [value];
        }

        value.forEach(function (v) {
          if (angular.isObject(v)) {
            v = angular.toJson(v);
          }

          url += (q ? '&' : '?') + encodeUri(key) + '=' + encodeUri(v);
          q = true;
        });
      });

      return url;
    }

    /* Apply a jsroute (from play's routeData) given the names of its arguments and optional data (or placeholders). */
    function getRoute(route, argNames, data) {
      var args;
      if (!argNames)
        argNames = [];
      var ph = arguments.length < 3;
      if (ph)
        args = _.map( argNames, function (a) { return ':' + a; });
      else if (data == null)
        args = [];
      else if (Array.isArray(data))
        args = data;
      else if (typeof data === 'object') {
        args = _.map(argNames, function (k) {
          return k in data ? data[k] : null;
        });
      } else
        args = [data];
      var r = route.apply(null, args);
      if (ph) {
        var q = r.url.indexOf('?');
        if (q !== -1)
          r.url = r.url.substr(0, q);
        r.url = r.url.replace(/%3A/gi, ':');
      }
      return r;
    }

    /* Add a route to $routeProvider and return a function to get a url given parameters. */
    function makeRoute(route, argNames, handler) {
      if (handler)
        $routeProvider.when(getRoute(route, argNames).url, handler);

      return function (data, params) {
        return urlParams(getRoute(route, argNames, data).url, params);
      };
    }

    function checkPermission($q, f, perm) {
      return f.then(function (obj) {
        return obj.checkPermission(perm) ? obj :
          $q.reject({status: 403});
      });
    }

    var routes = {};

    routes.index = makeRoute(controllers.SiteHtml.start, [], {
      controller: 'site/home',
      templateUrl: 'site/home.html',
      resolve: {
        volume: [
          'modelService', 'constantService',
          function (models, constants) {
            if (constants.sandbox) return; else
            return models.Volume.get(9, ['access'])
              .catch(function() {
                return {};
              });
          }
        ],
        tags: [
          'modelService',
          function (models) {
            return models.Tag.top();
          }
        ],
        activity: [
          'modelService',
          function (models) {
            return models.activity();
          }
        ]
      },
      reloadOnSearch: false,
    });

    //

    routes.login = makeRoute(controllers.LoginHtml.view, [], {
      controller: 'party/login',
      templateUrl: 'party/login.html',
      reloadOnSearch: false
    });

    //

    routes.register = makeRoute(controllers.LoginHtml.registration, [], {
      controller: 'party/register',
      templateUrl: 'party/register.html',
      resolve: {
        user: [
          'modelService',
          function (models) {
            if (models.Login.isLoggedIn())
              return models.Login.user.get(['parents']);
            else
              return models.Login.user;
          }
        ],
        token: function () {},
      },
      reloadOnSearch: false
    });

    //

    routes.password = makeRoute(controllers.TokenHtml.getPassword, [], {
      controller: 'party/reset',
      templateUrl: 'party/reset.html',
      reloadOnSearch: false
    });

    //

    routes.helpFormats = makeRoute(controllers.AssetHtml.formats, [], {
      controller: 'asset/formats',
      templateUrl: 'asset/formats.html',
      reloadOnSearch: false,
    });

    //

    makeRoute(controllers.TokenHtml.token, ['id'], {
      controller: 'party/register',
      templateUrl: 'party/register.html',
      resolve: {
        token: [
          'modelService', '$route',
          function (models, $route) {
            return models.Login.getToken($route.current.params.id, $route.current.params.auth);
          }
        ],
        user: [
          'modelService', function (models) {
            return models.Login.user;
          }
        ],
      },
      reloadOnSearch: false
    });

    //

    routes.volumeSearch = makeRoute(controllers.VolumeHtml.search, [], {
      controller: 'volume/search',
      templateUrl: 'volume/search.html',
      resolve: {
        volumes: [
          'pageService', function (page) {
            return page.models.Volume.search(page.$route.current.params);
          }
        ]
      },
    });

    //

    var partyView = {
      controller: 'party/view',
      templateUrl: 'party/view.html',
      resolve: {
        party: [
          'pageService', function (page) {
            var req = ['access', 'openid', 'parents', 'children', 'volumes'];
            if ('id' in page.$route.current.params)
              return page.models.Party.get(page.$route.current.params.id, req);
            else
              return page.models.Login.user.get(req);
          }
        ],
      },
      reloadOnSearch: false,
    };

    routes.profile = makeRoute(controllers.PartyHtml.profile, [], partyView);
    routes.party = makeRoute(controllers.PartyHtml.view, ['id'], partyView);

    //

    routes.partyEdit = makeRoute(controllers.PartyHtml.edit, ['id'], {
      controller: 'party/edit',
      templateUrl: 'party/edit.html',
      resolve: {
        party: [
          'pageService', function (page) {
            return checkPermission(page.$q,
              page.models.Party.get(page.$route.current.params.id, ['parents', 'children']),
              page.permission.EDIT);
          }
        ],
      },
      reloadOnSearch: false,
    });

    routes.partySearch = makeRoute(controllers.PartyHtml.search, [], {
      controller: 'party/search',
      templateUrl: 'party/search.html',
      resolve: {
        parties: [
          'pageService', function (page) {
            return page.models.Party.search(page.$route.current.params);
          }
        ]
      },
    });

    var volumeEdit = {
      controller: 'volume/edit',
      templateUrl: 'volume/edit.html',
      resolve: {
        volume: [
          'pageService', function (page) {
            if (!('id' in page.$route.current.params)) {
              return page.models.Login.user.get({'parents':'access'})
                .then(function () {
                  return undefined;
                });
            }

            return checkPermission(page.$q,
              page.models.Volume.get(page.$route.current.params.id,
                {access:'all', citation:'', links:'', top:'', funding:'', records:'', containers:'', tags:'keyword'}),
              page.permission.EDIT)
              .then(function (volume) {
                return volume.top.getSlot(volume.top.segment, ['assets'])
                  .then(function () {
                    return volume;
                  });
              });
          }
        ],
      },
      reloadOnSearch: false,
    };

    routes.volumeCreate = makeRoute(controllers.VolumeHtml.add, ['owner'], volumeEdit);
    routes.volumeEdit = makeRoute(controllers.VolumeHtml.edit, ['id'], volumeEdit);

    routes.volume = makeRoute(controllers.VolumeHtml.view, ['id'], {
      controller: 'volume/view',
      templateUrl: 'volume/view.html',
      resolve: {
        volume: [
          'pageService', function (page) {
            return page.models.Volume.get(page.$route.current.params.id,
              {access:'all', citation:'', links:'', funding:'', providers:'', consumers:'', top:'', tags:'', excerpts:'', comments:'', records:'', containers:'', assets:'top'});
          }
        ]
      },
      reloadOnSearch: false,
    });

    routes.volumeZip = makeRoute(controllers.VolumeController.zip, ['id'], {
      controller: 'volume/zip',
      templateUrl: 'volume/zip.html', 
      resolve: {
        volume: [
          'pageService', function (page) {
            return page.models.Volume.get(page.$route.current.params.id);
          }
        ],
        slot: function(){
        },
        fileList: [
          'pageService', function (page) {
            return page.router.http(page.router.controllers.VolumeApi.zipList, page.$route.current.params.id)
              .then(function (res) {
                return res.data;
              });
          }
        ],
      }
    });

    function slotRoute(edit) { return {
      controller: 'volume/slot',
      templateUrl: 'volume/slot.html',
      resolve: {
        slot: [
          'pageService', function (page) {
            var r = page.models.Volume.get(page.$route.current.params.vid, edit ? ['containers', 'records'] : []);
            return (edit ? checkPermission(page.$q, r, page.permission.EDIT) : r)
              .then(function (volume) {
                return volume.getSlot(page.$route.current.params.id, page.$route.current.params.segment,
                  ['releases', 'records', 'assets', 'excerpts', 'tags', 'comments']);
              });
          },
        ],
        edit: function () {
          return edit;
        }
      },
      reloadOnSearch: false,
    }; }
    routes.slot = makeRoute(controllers.SlotHtml.view, ['vid', 'id', 'segment'], slotRoute(false));
    routes.slotEdit = makeRoute(controllers.SlotHtml.edit, ['vid', 'id', 'segment'], slotRoute(true));

    routes.slotZip = makeRoute(controllers.SlotController.zip, ['vid', 'id', 'segment'], {
      controller: 'volume/zip',
      templateUrl: 'volume/zip.html', 
      resolve: {
        slot: [
          'pageService', function (page) {
            return page.models.Volume.get(page.$route.current.params.vid).then(function(v) {
              return v.getSlot(page.$route.current.params.id, page.$route.current.params.segment);
            }); 
          }
        ],
        volume: function () {
        },
        fileList: [
          'pageService', 'Segment', function (page, Segment) {
            return page.router.http(page.router.controllers.SlotApi.zipList, page.$route.current.params.vid,
              page.$route.current.params.id, Segment.format(page.$route.current.params.segment))
              .then(function (res) {
                return res.data;
              });
          }
        ],
      }
    });

    routes.slotAsset = makeRoute(controllers.AssetSlotHtml.view, ['vid', 'cid', 'segment', 'id'], {
      controller: 'asset/view',
      templateUrl: 'asset/view.html',
      resolve: {
        asset: [
          'pageService', function (page) {
            return page.models.Volume.get(page.$route.current.params.vid)
              .then(function (volume) {
                return volume.getAsset(page.$route.current.params.id, page.$route.current.params.cid, page.$route.current.params.segment);
              });
          },
        ]
      },
      reloadOnSearch: false,
    });

    routes.record = makeRoute(controllers.RecordHtml.view, ['id']);
    routes.volumeThumb = makeRoute(controllers.VolumeController.thumb, ['id', 'size']);
    routes.volumeCSV = makeRoute(controllers.VolumeController.csv, ['id']);
    routes.assetThumb = makeRoute(controllers.AssetSlotController.thumb, ['cid', 'segment', 'id', 'size']);
    routes.assetDownload = makeRoute(controllers.AssetSlotController.download, ['cid', 'segment', 'id', 'inline']);
    routes.partyAvatar = makeRoute(controllers.PartyHtml.avatar, ['id', 'size']);
    routes.assetEdit = makeRoute(controllers.AssetHtml.edit, ['id']);
    routes.recordEdit = makeRoute(controllers.RecordHtml.edit, ['id']);

    //

    this.$get = [
      '$rootScope', '$location', '$http', '$cacheFactory', 'constantService', 'analyticService',
      function ($rootScope, $location, $http, $cacheFactory, constants, analytics) {
        var router = angular.extend({
          controllers: controllers,
          prev: '/'
        }, routes);

        /* Simple wrapper to $http(route(...)).
         * Non-object arguments (or an initial array) are passed to route.
         * The next argument is passed as data (POST) or params.
         * An additional argument is passed as extra config to $http.
         */
        router.http = function (route, args/*...*/) {
          var i;
          if (Array.isArray(args))
            i = 2;
          else {
            for (i = 1; i < arguments.length; i ++)
              if (angular.isObject(arguments[i]))
                break;
            args = Array.prototype.slice.call(arguments, 1, i);
          }
          var r = route.apply(null, args);
          if (i < arguments.length) {
            if (r.method === 'POST' || r.method === 'PUT')
              r.data = arguments[i];
            else
              r.params = arguments[i];
            if (++i < arguments.length)
              angular.extend(r, arguments[i]);
          }
          if (r.data instanceof FormData) {
            if (!('transformRequest' in r))
              r.transformRequest = angular.identity;
            if (!('headers' in r))
              r.headers = {};
            // Not sure why we do this but it seems to be necessary:
            if (!('Content-Type' in r.headers))
              r.headers['Content-Type'] = undefined;
          }
          var cache;
          /* try to guess (conservatively) if this request is cached: */
          if (r.method === 'GET' || r.method === 'JSONP') {
            cache = r.cache || $http.defaults.cache && $cacheFactory('$http');
            if (cache && !r.params)
              cache = cache.get(r.url);
          }
          if (!cache) {
            var a = analytics.dump();
            if (a) {
              if (!('headers' in r))
                r.headers = {Analytics: a};
              else
                r.headers.Analytics = a;
            }
          }
          return $http(r);
        };

        router.permalink = function (url) {
          return constants.url + url;
        };

        var prev;
        $rootScope.$on('$locationChangeStart', function (event, to, from) {
          router.prev = prev;
          prev = from;
        });
        $rootScope.$on('$routeChangeStart', function () {
          router.prev = prev;
        });

        router.back = function () {
          if (!router.prev || router.prev === $location.absUrl())
            $location.url('/');
          else
            $location.$$parse(router.prev);
        };

        return router;
      }
    ];
  }
]);
