# performanceScores: ranking and Mean average precision.
performanceScores <- function(distanceMtx, POS) {
  N <- ncol(POS)
  rankingSum <- 0
  Av.Precision <- 0
  for (i in 1:N) {
    # sum of ranking
    distanceMtx_i <- distanceMtx[POS[, i][2], ]
    distanceMtx_i[matrix(POS[matrix(rep(POS[1, ] == POS[, i][1], 2), nrow = 2 , byrow = TRUE)], nrow = 2)[2,]] = Inf
    rankingSum <- rankingSum + which(order(distanceMtx_i) == POS[, i][1])

    # Average precision
    trueVec <- rep(0, ncol(distanceMtx))
    trueVec[POS[, i][1]] <- 1
    Av.Precision <- Av.Precision + averagePrecision(length(trueVec), trueVec, -distanceMtx_i)
  }
  ranking <- rankingSum/N
  MAP <- Av.Precision/N
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
