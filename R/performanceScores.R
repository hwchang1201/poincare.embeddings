# performanceScores: ranking and Mean average precision.
performanceScores <- function(distanceMtx, POS) {

  return(list(ranking=ranking, MAP=MAP))
}

# Average Precision.
averagePrecision <- function (uNumToExamineK, Yreal, Yhat){

  # The real Y values is sorted by predicted Y values in decending order(decreasing=TRUE)
  Yreal_sort_d <- Yreal[order(Yhat, decreasing=TRUE)]
  Yreal_sort_d <- Yreal_sort_d[1:uNumToExamineK]
  averagePrecision <- sum(cumsum(Yreal_sort_d) * Yreal_sort_d / seq_along(Yreal_sort_d)) / sum(Yreal_sort_d)
  return(averagePrecision)
}
