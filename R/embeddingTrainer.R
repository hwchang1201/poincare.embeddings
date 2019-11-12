# define poincare distance between two vectors.
#' Poincare distance between two vectors
#'
#' @param theta_i - A N x 1 vector with N : Embedding space dimension. Each of theta works as proxy of an entity in tree-shape dataset.
#' @param theta_j - A N x 1 vector with N : Embedding space dimension. Each of theta works as proxy of an entity in tree-shape dataset.
#'
#' @return A poincare distance between theta_i and theta_j.
#' @export
#'
#' @examples getPoincareDistanceVec(c(0, 0), c(0.1, 0.5))
#' @examples getPoincareDistanceVec(c(0.4, 0), c(0.1, 0.5))
getPoincareDistanceVec <- function(theta_i, theta_j) { # input : two vectors
  STABILITY <- 1e-4
  alpha <- 1 - as.numeric(crossprod(theta_i))
  beta <- 1 - as.numeric(crossprod(theta_j))
  gamma <- 1 + 2 / (alpha * beta + STABILITY) * as.numeric(crossprod(theta_i - theta_j))
  distance <- acosh(gamma)

  return(distance) # d(theta_i, theta_j)
}
# define poincare distance in matrix.
#' Poincare distance in matrix
#'
#' @param theta - A N x M matrix with N : the number of entities and M : dimension of the embedding space.
#'
#' @return A N x N poincare distance matrix whose (i, j) element is poincare distance between theta_i and theta_j.
#' @export
#'
#' @examples getPoincareDistance(matrix(stats::rnorm(100), 5, 20))
getPoincareDistance <- function(theta) { # input : matrix theta
  STABILITY <- 1e-5
  N <- dim(theta)[1] # data point of theta.
  p <- dim(theta)[2] # number of dimension of embedding space.

  rowWiseDuplicate <- do.call(rbind, replicate(N, t(theta), simplify=FALSE))
  columnWiseDuplicate <- do.call(cbind, replicate(N, as.vector(t(theta)), simplify=FALSE))
  diffSquare <- (rowWiseDuplicate - columnWiseDuplicate)^2

  crossDistance <- matrix(rep(0, N * N), N, N)
  for (i in 1:N){
    crossDistance[i, ] <- colSums(diffSquare[(p*i-(p-1)):(p*i), ])
  }
  distanceMtx <- acosh(1 + 2 * crossDistance / (tcrossprod(1 + STABILITY - rowSums(theta^2))))

  return(distanceMtx) # N by N distance matrix with ij element = d(theta_i, theta_j)
}


# projection function
#' Projection function: to make thetas stay in the unit ball
#'
#' @param  - A N x 1 vector with N : Embedding space dimension. Each of theta works as proxy of an entity in tree-shape dataset.
#'
#' @return - A N x 1 vector inside the unit ball.
#' @export
#'
#' @examples proj(c(2,2))
proj <- function(theta_i) { #input : should be one row(vector) of theta matrix

  STABILITY <- 1e-5

  if (as.numeric(crossprod(theta_i)) >= 1) {
    theta_i <- theta_i / sqrt(as.numeric(crossprod(theta_i))) - STABILITY
  }

  return(theta_i)
}


#get Euclidean Gradient w.r.t theta
#' Euclidean Gradient of distance function with respect to theta_i
#'
#' @param theta_i - A N x 1 vector with N : Embedding space dimension. Each of theta works as proxy of an entity in tree-shape dataset.
#' @param theta_j - A N x 1 vector with N : Embedding space dimension. Each of theta works as proxy of an entity in tree-shape dataset.
#'
#' @return d(dist(theta_i, theta_j)) / d(theta_i) : A N x 1 vector.
#' @export
#'
#' @examples getDistanceGradVec(c(0, 0), c(0.1, 0.5))
getDistanceGradVec <- function(theta_i, theta_j) {
  STABILITY = 1e-5
  alpha <- 1 - as.numeric(crossprod(theta_i))
  beta <- 1 - as.numeric(crossprod(theta_j))
  gamma <- 1 + 2 / (alpha * beta) * as.numeric(crossprod(theta_i - theta_j))

  distanceGradVec <- 4 / (beta * sqrt(gamma^2 - 1) + STABILITY) *
    ((as.numeric(crossprod(theta_j)) - 2 * as.numeric(crossprod(theta_i, theta_j)) + 1)/ (alpha^2 + STABILITY) * theta_i - theta_j / (alpha + STABILITY))

  return(distanceGradVec)
}


# embedding trainer -> output : trained theta.
#' Poincare-embedding trainer
#'
#' @param POS - A 2-row positive relation matrix that contains positive relation entities columnwisely.
#' @param NEG - A 2-row negative relation matrix that contains negative relation entities columnwisely.
#' @param entity - A vector of all the entities in the tree-shape dataset.
#' @param theta_dim - The dimension of the embedding space.
#' @param N_epoch - The number of epochs.
#' @param lr - The learning rate.
#' @param n_neg - The number of negative samples for each iteration.
#'
#' @return A trained matrix theta.
#' @export
#'
#' @examples statistics_yaml <- yaml::yaml.load(statistics)
#' @examples statistics_tree <- data.tree::as.Node(statistics_yaml)
#' @examples dataset <- dataLoader(statistics_tree)
#' @examples embeddingTrainer(dataset$POS, dataset$NEG, dataset$entity, 2, 100, 0.001, 5)
embeddingTrainer <- function(POS, NEG, entity, theta_dim=2, N_epoch=100, lr=0.001, n_neg=4){

  # Initializing theta
  theta <- matrix(runif(theta_dim * length(entity), min = -0.001, max = 0.001), ncol = theta_dim)

  for (epoch in 1:N_epoch) {
    # stochastic gradient method
    if (epoch == N_epoch %/% 10){
      lr = 10 * lr
    }
    parent_node_length <- length(unique(POS[1, ]))
    for (k in 1:parent_node_length) {
      Pos_now <- matrix(POS[matrix(rep(POS[1, ] == k, 2), nrow = 2 , byrow = TRUE)], nrow = 2)
      j <- sample(1:ncol(Pos_now), 1)
      pos <- Pos_now[, j]
      # # pick one vector from positive relation matrix
      # #j <- sample(1:ncol(D), 1)
      # pos <- D[, k]
      # distance between theta[pos[1]], theta[pos[2]]
      dist_p <- getPoincareDistanceVec(theta[pos[1], ], theta[pos[2]])
      # derivative of loss function w.r.t distance between theta[pos[1]], theta[pos[2]]
      der_p <- -1
      # empty negative relation sampling matrix
      neg_mat <- matrix(rep(0, 2 * n_neg), ncol = n_neg)
      # empty vector to store distance between theta[pos[1]], theta[neg[j]] j = 1, 2, .., n_neg
      dist_neg <- rep(0, n_neg)
      # making neg_mat, dist_neg
      for (i in 1:n_neg) {
        # negative relation matrix subsetting by NEG[1, ] == pos[1]
        Neg_now <- matrix(NEG[matrix(rep(NEG[1, ] == pos[1], 2), nrow = 2 , byrow = TRUE)], nrow = 2)
        j <- sample(1:ncol(Neg_now), 1)
        neg_mat[, i] <- Neg_now[, j]
        dist_neg[i] <- getPoincareDistanceVec(theta[neg_mat[, i][1], ], theta[neg_mat[, i][2], ])
      }
      # denominator of loss function.
      denom <- sum(exp(-dist_neg))
      # making der_neg : derivative of loss function w.r.t distance between theta[pos[1]], theta[neg[j]], j = 1, 2, ..., n_neg.
      der_neg <- rep(0, n_neg)
      for (i in 1:n_neg) {
        der_neg[i] <- exp(-dist_neg[i])/denom
      }

      # update theta[pos[1], ], theta[pos[2], ].
      theta[pos[1], ] <- proj(theta[pos[1], ] + lr  * (1 - as.numeric(crossprod(theta[pos[1], ])))^2 / 4 * der_p * getDistanceGradVec(theta[pos[1], ], theta[pos[2], ]))
      theta[pos[2], ] <- proj(theta[pos[2], ] + lr  * (1 - as.numeric(crossprod(theta[pos[2], ])))^2 / 4 * der_p * getDistanceGradVec(theta[pos[2], ], theta[pos[1], ]))

      # update theta[pos[1], ], theta[neg[j], ], j = 1, 2, ..., n_neg.
      for (i in 1:n_neg) {
        theta[neg_mat[, i][1], ] <- proj(theta[neg_mat[, i][1], ] + lr * (1 - as.numeric(crossprod(theta[neg_mat[, i][1], ])))^2 / 4 * der_neg[i] * getDistanceGradVec(theta[neg_mat[, i][1], ], theta[neg_mat[, i][2], ]))
        theta[neg_mat[, i][2], ] <- proj(theta[neg_mat[, i][2], ] + lr * (1 - as.numeric(crossprod(theta[neg_mat[, i][2], ])))^2 / 4 * der_neg[i] * getDistanceGradVec(theta[neg_mat[, i][2], ], theta[neg_mat[, i][1], ]))
      }
    }
  }

  return(theta)
}
