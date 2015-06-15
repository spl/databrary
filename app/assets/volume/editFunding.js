'use strict';

app.directive('volumeEditFundingForm', [
  'pageService','$q', function (page, $q) {
    var link = function ($scope) {
      var volume = $scope.volume;
      var form = $scope.volumeEditFundingForm;

      form.data = volume.funding.slice();

      var subforms = [];

      form.saveAll = function () {
        form.$setSubmitted();

        // Kickstart all the subforms. 
        var formPromises = _.map(subforms, function(subform){
          if (subform.$dirty){
            return subform.save(false);
          }
        });


        // Due to the fact that lodash doesn't automatically
        // discard the `undefined` values. 
        formPromises = _.compact(formPromises);

        // Once all the forms are done, we need to set the state to
        // unsubmitted
        $q.all(formPromises).then(function(){
          form.$setUnsubmitted();
        }, function(){
          form.$setUnsubmitted();
        });
      };

      $scope.$on('fundingGrantForm-init', function (event, grantForm) {
        subforms.push(grantForm);

        grantForm.removeSuccessFn = function (funding) {
          form.data.remove(funding);
          subforms.remove(grantForm);
        };
      });

      $scope.selectFn = function (found) {
        page.messages.clear(form);
        if (form.data.some(function (funding) {
              return funding.funder.id === found.id;
            })) {
          page.messages.add({
            type: 'yellow',
            body: page.constants.message('funding.search.repeat', found.name),
            owner: form
          });
          return;
        }

        form.data.push({
          funder: found,
          awards: [],
          new: true,
        });

        //warning: next line is template dependent! if classnames or structure change this may no longer work
        page.display.scrollTo('fieldset.funding-grant:last');
      };
    };

    //

    return {
      restrict: 'E',
      templateUrl: 'volume/editFunding.html',
      link: link
    };
  }
]);
