'use strict'

app.directive 'volumeOverview', [
  'constantService',
  (constants) ->
    generateSummary = (volume) ->
      volume.summary = summary =
        sessions: 0
        shared: 0
        agemin: Infinity
        agemax: -Infinity
        agesum: 0
        ages: 0
        participants: 0

      Participant = constants.categoryName.participant
      Gender = Participant.metricName.gender.id
      Participant = Participant.id
      for ci, c of volume.containers when !c.top
        summary.sessions++
        summary.shared++ if c.release >= constants.release.SHARED
        for r in c.records when 'age' of r and volume.records[r.id].category == Participant
          if r.age < summary.agemin
            summary.agemin = r.age
          if r.age > summary.agemax
            summary.agemax = r.age
          summary.agesum += r.age
          summary.ages++
      summary.agemean = summary.agesum / summary.ages

      for ri, r of volume.records
        if r.category == Participant
          summary.participants++
          if g = r.measures[Gender]
            summary.genders ||= {}
            if summary.genders[g]
              summary.genders[g]++
            else
              summary.genders[g] = 1

    {
    restrict: 'E'
    templateUrl: 'volume/overview.html'
    scope: false
    link:
      pre: ($scope) ->
        generateSummary($scope.volume) unless $scope.volume.summary
        return
    }
]
