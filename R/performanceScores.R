#' Getting Performance Scores : Ranking and Mean average precision.
#'
#' This function gives mean ranking and mean average precision of the trained embedding entities, which are two main performance measures of graph model.
#' Please refer to 'vignette' to find detailed concepts of Ranking and Mean average precision.
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

# performanceScores: ranking and Mean average precision.
performanceScores <- function(distanceMtx, POS) {
  N <- ncol(POS) # number of column of Positive set.
  rankingSum <- 0 # to add the ranking of individual estimate.
  Av.Precision <- 0 # to add the average precision of individual estimate.
  for (i in 1:N) {
    # sum of ranking
    distanceMtx_i <- distanceMtx[POS[, i][2], ] # poincare distance matrix subsetted by i-th theta.
    # nodes with sharing parent node should not be in the ranking.
    distanceMtx_i[matrix(POS[matrix(rep(POS[1, ] == POS[, i][1], 2), nrow = 2 , byrow = TRUE)], nrow = 2)[2,]] = Inf
    # get the ranking of parent node of theta_i.
    rankingSum <- rankingSum + which(order(distanceMtx_i) == POS[, i][1])

    # Average precision
    trueVec <- rep(0, ncol(distanceMtx)) # we label the parent node of theta_i equal to 1, otherwise 0.
    trueVec[POS[, i][1]] <- 1 # we label the parent node of theta_i equal to 1, otherwise 0.
    Av.Precision <- Av.Precision + averagePrecision(length(trueVec), trueVec, -distanceMtx_i) # get average precision based on distance score.
  }
  ranking <- rankingSum/N #average
  MAP <- Av.Precision/N #average
  return(list(ranking=ranking, MAP=MAP))
}
# Average Precision.
#' Getting Average precision.
#'
#' This function gives average precision.
#' Please refer to 'vignette' to find detailed concept of average precision.
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
  Yreal_sort_d <- Yreal[order(Yhat, decreasing=TRUE)] # The real Y values is sorted by predicted Y values in decending order(decreasing=TRUE)
  Yreal_sort_d <- Yreal_sort_d[1:uNumToExamineK] #cut by uNumToExamineK, in our case use them all.
  averagePrecision <- sum(cumsum(Yreal_sort_d) * Yreal_sort_d / seq_along(Yreal_sort_d)) / sum(Yreal_sort_d) # get average precision.
  return(averagePrecision)
}
