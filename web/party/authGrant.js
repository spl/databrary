'use strict';

app.directive('authGrantForm', [
  'pageService', function (page) {
    var link = function ($scope) {
      var party = $scope.party;
      var auth = $scope.auth;
      var form = $scope.authGrantForm;

      form.data = {
        site: auth.site,
        member: auth.member,
        expires: auth.expires && page.$filter('date')(auth.expires, 'yyyy-MM-dd')
      };

      if (auth.new)
        form.$setDirty();

      //

      form.presetName = function (type, name, party) {
        return '<strong>' + page.constants.message('auth.' + type + '.' + name + '.title') + '</strong>: ' + page.$filter('possessive')('auth.' + type + '.' + name, party);
      };

      $scope.canGrantSite = function (p) {
        return p == page.permission.NONE ||
          p == page.permission.READ ||
          p > page.permission.READ &&
          page.models.Login.checkAuthorization(p + 1) ||
          page.models.Login.checkAuthorization(page.permission.ADMIN);
      };

      $scope.canGrantMember = function (p) {
        return p == page.permission.NONE ||
          p == page.permission.READ ||
          p == page.permission.EDIT ||
          p == page.permission.ADMIN ||
          page.models.Login.checkAuthorization(page.permission.ADMIN);
      };

      //

      form.save = function () {
        page.messages.clear(form);
        return party.authorizeSave(auth.party.id, form.data).then(function () {
          form.validator.server({});
          page.messages.add({
            body: page.constants.message('auth.grant.save.success'),
            type: 'green',
            owner: form
          });

          delete auth.new;
          form.$setPristine();
        }, function (res) {
          form.validator.server(res);
          page.display.scrollTo(form.$element);
        });
      };

      //

      form.deny = function () {
        page.messages.clear(form);
        if (auth.new) {
          form.denySuccessFn(auth);
          return;
        }
        party.authorizeRemove(auth.party.id).then(function () {
          form.validator.server({});
          page.messages.add({
            body: page.constants.message('auth.grant.remove.success'),
            type: 'green',
            owner: form
          });
          form.$setPristine();
          form.denySuccessFn(auth);
        }, function (res) {
          form.validator.server(res);
          page.display.scrollTo(form.$element);
        });
      };

      //

      form.validator.client({
        expires: {
          tips: page.constants.message('auth.grant.expires.help')
        }
      }, true);

      //

      $scope.$emit('authGrantForm-init', form);
    };

    //

    return {
      restrict: 'E',
      templateUrl: 'party/authGrant.html',
      link: link,
    };
  }
]);

