'use strict';

app.directive('volumeMeterCanvas', [
  function () {
    return {
      restrict: 'A',
      link: function($scope, $element, $attrs) {
        var meterCanvas = $element[0];
        meterCanvas.height = 600;
        meterCanvas.width = 600;
        var meterContext = meterCanvas.getContext('2d');

        $scope.$watch('meter',function(){
          var radius = Math.floor(meterCanvas.width/2*0.8);

          meterContext.beginPath();
          meterContext.lineWidth=48;
          meterContext.arc(meterCanvas.width/2,meterCanvas.height/2,radius,0,2*Math.PI);
          meterContext.strokeStyle="#CECECE";
          meterContext.stroke();
          meterContext.closePath();

          meterContext.beginPath();
          meterContext.arc(meterCanvas.width/2,meterCanvas.height/2,radius,-Math.PI/2,2*Math.PI*$scope.meter-Math.PI/2);
          meterContext.strokeStyle="#5CAABC";
          meterContext.stroke();
          meterContext.closePath();

          meterContext.textAlign = "center";
          meterContext.textBaseline = "middle";
          meterContext.fillStyle = "#333333";
          meterContext.font = "30px Verdana";
          meterContext.fillText('Your volume is',meterCanvas.width/2, meterCanvas.height/2-60);
          meterContext.font = "72px Verdana";
          meterContext.fillText((100*$scope.meter).toFixed()+'%',meterCanvas.width/2, meterCanvas.height/2+15);
          meterContext.font = "36px Verdana";
          meterContext.fillText('complete',meterCanvas.width/2, meterCanvas.height/2+80);
        });
      }
    };
  }]);