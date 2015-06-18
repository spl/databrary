'use strict';

app.directive('ngForm', [
  '$route', '$timeout', '$animate', 'constantService', 'messageService',
  function ($route, $timeout, $animate, constants, messages) {
    var pre = function ($scope, $element, $attrs) {
      var name = $attrs.name || $attrs.ngForm;
      var form = name && $scope.$eval(name);
      if (!form)
        return;

      form.$element = $element;

      var controls = [];

      function checkDirty() {
        if (controls.every(function (control) {
            return control.$pristine;
          })) {
          /* effectively call form.$setPristine, without the controls. */
          $animate.removeClass($element, 'ng-dirty');
          $animate.addClass($element, 'ng-pristine');
          form.$dirty = false;
          form.$pristine = true;
        }
      }

      var $addControl = form.$addControl;
      var $removeControl = form.$removeControl;

      /* this is unfortunate, just because we can't access the existing controls list. */
      form.$addControl = function (control) {
        if ('$pristine' in control)
          controls.push(control);
        return $addControl(control);
      };

      // This method sets the form state back to its unsubitted state
      form.$setUnsubmitted = function () {

        // remove the angular helper classes that `setSubmit` creates.
        $animate.removeClass($element, 'ng-submitted');

        // actually set the state of the form.
        form.$submitted = false;
      };

      form.$removeControl = function (control) {
        controls.remove(control);
        checkDirty();
        return $removeControl(control);
      };

      form.subformControl = {
        $setPristine: checkDirty
      };

      if ('isolate' in $attrs) {
        form.$$parentForm.$removeControl(form);
        form.$setDirty = function() {
          $animate.removeClass($element, 'ng-pristine');
          $animate.addClass($element, 'ng-dirty');
          form.$dirty = true;
          form.$pristine = false;
        };
      }
      /* it'd be nicer to handle this in $addControl, but it happens too early */
      else if (form.$$parentForm.subformControl)
        form.$addControl(form.$$parentForm.subformControl);

      var unclaimed = {};
      form.validators = {};
      form.validator = {
        server: function (res, replace) {
          messages.clear(form);
          if ($.isEmptyObject(res)) {
            res.data = {};
          } else if (!angular.isObject(res.data)) {
            messages.addError({
              body: constants.message('error.generic'),
              report: res,
              owner: form
            });
            return;
          }

          var name;
          for (name in form.validators) {
            form.validators[name].server(res.data[name] || {}, replace);
          }

          for (name in res.data) {
            if (form.validators[name]) {
              form.validators[name].server(res.data[name], replace);
            } else {
              messages.add({
                type: 'red',
                body: Array.isArray(res.data[name]) ? res.data[name].join(', ') : res.data[name],
                owner: form
              });
            }
          }
        },

        clearServer: function () {
          _.each(form.validators, function (validator) {
            validator.server({}, true);
          });
        },

        client: function (data, replace) {
          for (var name in data) {
            if (!data.hasOwnProperty(name)) {
              continue;
            } else if (form.validators[name]) {
              form.validators[name].client(data[name], replace);
            } else {
              unclaimed[name] = data[name];
            }
          }
        },

        add: function (name, validator) {
          form.validators[name] = validator;

          if (unclaimed[name]) {
            validator.client(unclaimed[name], true);
            delete unclaimed[name];
          }
        },
      };

      form.resetAll = function (force, check) {
        if (!(force || form.$pristine || confirm(constants.message('navigation.confirmation'))))
          return false;
        if (check)
          return true;
        var x = window.pageXOffset,
            y = window.pageYOffset;
        $route.reload();
        $timeout(function () {
          window.scrollTo(x, y);
        });
        return true;
      };
    };

    return {
      restrict: 'EA',
      link: {
        pre: pre
      }
    };
  }
]);
