# performanceScores: ranking and Mean average precision.
#' Title
#'
#' @param distanceMtx - A N x N poincare distance matrix whose (i, j) element is poincare distance between theta_i and theta_j.
#' @param POS - A 2-row positive relation matrix that contains positive relation entities columnwisely.
#'
#' @return Ranking : The ranking of the true related entity.
#' @return MAP : Mean average precision.
#' @export
#'
#' @examples toy_yaml <- yaml::yaml.load(toy)
#' @examples toy_tree <- data.tree::as.Node(toy_yaml)
#' @examples toy_dataset <- dataLoader(toy_tree)
#' @examples emb <- poincareEmbeddings(toy_tree, 2, 50, 0.001, 5)
#' @examples distanceMtx <- getPoincareDistance(emb$theta)
#' @examples performanceScores(distanceMtx, toy_dataset$POS)
#'
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
#' Average precision
#'
#' @param uNumToExamineK - The number of element of Yreal.
#' @param Yreal - A N x 1 vector that indicates that "1" is true, "0" is false.
#' @param Yhat - The estimated score(possibility) to be labeled as "1". The higher the element value is, The more likely to be labeled as "1".
#'
#' @return average precision
#' @export
#'
#' @examples averagePrecision(2, c(1,0), c(0.5, 0.3))
#' @examples averagePrecision(2, c(0,1), c(0.5, 0.3))
averagePrecision <- function (uNumToExamineK, Yreal, Yhat){

  # The real Y values is sorted by predicted Y values in decending order(decreasing=TRUE)
  Yreal_sort_d <- Yreal[order(Yhat, decreasing=TRUE)]
  Yreal_sort_d <- Yreal_sort_d[1:uNumToExamineK]
  averagePrecision <- sum(cumsum(Yreal_sort_d) * Yreal_sort_d / seq_along(Yreal_sort_d)) / sum(Yreal_sort_d)
  return(averagePrecision)
}
