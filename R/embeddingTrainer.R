#' Calculating Poincare distance between two vectors.
#'
#' This function measures Poincare distance between two vectors.
#' Please refer to 'vignette' to find the equation for the Poincare distance and detailed ideas.
#'
#' @param theta_i - A N x 1 vector with N : Embedding space dimension. Each of theta works as proxy of an entity in tree-shape dataset.
#' @param theta_j - A N x 1 vector with N : Embedding space dimension. Each of theta works as proxy of an entity in tree-shape dataset.
#'
#' @return A poincare distance between theta_i and theta_j.
#' @export
#'
#' @examples getPoincareDistanceVec(c(0, 0), c(0.1, 0.5))
#' @examples getPoincareDistanceVec(c(0.4, 0), c(0.1, 0.5))
# define poincare distance between two vectors.
getPoincareDistanceVec <- function(theta_i, theta_j) { # input : two vectors
  STABILITY <- 1e-5 # for gamma not to be Inf.
  alpha <- 1 - as.numeric(crossprod(theta_i)) # write down the alpha in the paper.
  beta <- 1 - as.numeric(crossprod(theta_j)) # write down the beta in the paper.
  gamma <- 1 + 2 / (alpha * beta + STABILITY) * as.numeric(crossprod(theta_i - theta_j)) # write down the gamma in the paper.
  distance <- acosh(gamma) # definition of poincare distance.

  return(distance) # d(theta_i, theta_j)
}
#' Calculating Poincare distance matrix
#'
#' This function gives a matrix as an output, whose (i ,j) element measures Poincare distance between i-th row vector and j-th row vector of the input matrix.
#' Please refer to 'vignette' to find the equation for the Poincare distance Matrix and detailed ideas.
#' @param theta - A N x M matrix with N : the number of entities and M : dimension of the embedding space.
#'
#' @return A N x N poincare distance matrix whose (i, j) element is poincare distance between theta_i and theta_j.
#' @export
#'
#' @examples getPoincareDistance(matrix(stats::rnorm(100), 5, 20))
# define poincare distance in matrix.
getPoincareDistance <- function(theta) { # input : matrix theta
  STABILITY <- 1e-5 # for distanceMtx not to be Inf.
  N <- dim(theta)[1] # data point of theta.
  p <- dim(theta)[2] # number of dimension of embedding space.

  rowWiseDuplicate <- do.call(rbind, replicate(N, t(theta), simplify=FALSE)) # row wise duplicate of the embedding theta.(N * p by N matrix)
  columnWiseDuplicate <- do.call(cbind, replicate(N, as.vector(t(theta)), simplify=FALSE)) # column wise duplicate of the embedding theta. (N * p by N matrix)
  diffSquare <- (rowWiseDuplicate - columnWiseDuplicate)^2 # (N * p by N matrix)

  crossDistance <- matrix(rep(0, N * N), N, N)
  for (i in 1:N){
    crossDistance[i, ] <- colSums(diffSquare[(p*i-(p-1)):(p*i), ]) # column sum of i-th row from i = 1, ..., N
  }
  # N by N matrix whose (i, j) element is poincare distance between ith and jth
  distanceMtx <- acosh(1 + 2 * crossDistance / (tcrossprod(1 + STABILITY - rowSums(theta^2))))
  return(distanceMtx) # N by N distance matrix with ij element = d(theta_i, theta_j)
}


#' Projection function.
#'
#' This function make each embedding point theta stay in the unit ball by normalize the vector when Euclidean norm of the point is greater than 1.
#' Please refer to 'vignette' to find the equation for the projection function and detailed ideas.
#' @param  - A N x 1 vector with N : Embedding space dimension. Each of theta works as proxy of an entity in tree-shape dataset.
#'
#' @return - A N x 1 vector inside the unit ball.
#' @export
#'
#' @examples project(c(2,2))
# projection function
project <- function(theta_i) { #input : should be one row(vector) of theta matrix

  STABILITY <- 1e-5 # for norm of theta_i not to be over 1.

  if (as.numeric(crossprod(theta_i)) >= 1) {
    theta_i <- theta_i / sqrt(as.numeric(crossprod(theta_i))) - STABILITY # normalized theta_i if norm of theta_i is greater than 1.
  }

  return(theta_i)
}


#' Calculating Euclidean Gradient of distance function with respect to theta_i
#'
#' This function calculates Euclidean Gradient of distance between theta_i and theta_j with respect to theta_i(the first argment of the function).
#' Please refer to 'vignette' to find the equation for Euclidean Gradient function and detailed ideas.
#'
#' @param theta_i - A N x 1 vector with N : Embedding space dimension. Each of theta works as proxy of an entity in tree-shape dataset.
#' @param theta_j - A N x 1 vector with N : Embedding space dimension. Each of theta works as proxy of an entity in tree-shape dataset.
#'
#' @return d(dist(theta_i, theta_j)) / d(theta_i) : A N x 1 vector.
#' @export
#'
#' @examples getDistanceGradVec(c(0, 0), c(0.1, 0.5))
#get Euclidean Gradient w.r.t theta
getDistanceGradVec <- function(theta_i, theta_j) {
  STABILITY = 1e-5 # for distanceGradVec not to be Inf.
  alpha <- 1 - as.numeric(crossprod(theta_i)) # alpha in paper
  beta <- 1 - as.numeric(crossprod(theta_j)) # beta in paper
  gamma <- 1 + 2 / (alpha * beta) * as.numeric(crossprod(theta_i - theta_j)) # gamma in paper
  # refer the vinette for the equation.
  distanceGradVec <- 4 / (beta * sqrt(gamma^2 - 1) + STABILITY) *
    ((as.numeric(crossprod(theta_j)) - 2 * as.numeric(crossprod(theta_i, theta_j)) + 1)/ (alpha^2 + STABILITY) * theta_i - theta_j / (alpha + STABILITY))

  return(distanceGradVec)
}


#' Poincare-embedding trainer
#'
#' This function performs optimization with respect to the objective function.
#' Please refer to 'vignette' to find the equation for the objective function and detailed ideas(e.g. how to optimize).
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

# embedding trainer -> output : trained theta.
embeddingTrainer <- function(POS, NEG, entity, theta_dim=2, N_epoch=100, lr=0.001, n_neg=4){

  # Initializing theta(near zero, uniform distributed.)
  theta <- matrix(runif(theta_dim * length(entity), min = -0.001, max = 0.001), ncol = theta_dim)
  # for the burn-in period. For good initial angular layout.
  lr = lr / 10 # reduced learning rate to 10% of it.
  for (epoch in 1:N_epoch) {
    # stochastic gradient method
    if (epoch == N_epoch %/% 10){
      lr = 10 * lr # recover learning rate after (total epochs) / 10
    }
    parent_node_length <- length(unique(POS[1, ])) # now many parent notes are.
    for (k in 1:parent_node_length) {
      #iterate one by one for each parent node.
      Pos_now <- matrix(POS[matrix(rep(POS[1, ] == k, 2), nrow = 2 , byrow = TRUE)], nrow = 2) # subset the whole positive set for only k-th parent node.
      j <- sample(1:ncol(Pos_now), 1) # randomly select one index of child node
      pos <- Pos_now[, j] # coupled one parent and selected child positive relation
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
        Neg_now <- matrix(NEG[matrix(rep(NEG[1, ] == pos[1], 2), nrow = 2 , byrow = TRUE)], nrow = 2) # subset the whole negative set for only k-th(pos[1]) parent node.
        j <- sample(1:ncol(Neg_now), 1) # randomly select one index negative relation
        neg_mat[, i] <- Neg_now[, j] # make negative couple for pos[1] parent and selected node above.
        # distance between pos[1] node and selected negative node.
        dist_neg[i] <- getPoincareDistanceVec(theta[neg_mat[, i][1], ], theta[neg_mat[, i][2], ])
      }
      # denominator of loss function.
      denom <- sum(exp(-dist_neg))
      # making der_neg : derivative of loss function w.r.t distance between theta[pos[1]], theta[neg[j]], j = 1, 2, ..., n_neg.
      der_neg <- rep(0, n_neg)
      for (i in 1:n_neg) {
        # making der_neg : derivative of loss function w.r.t distance between theta[pos[1]], theta[neg[j]], j = 1, 2, ..., n_neg.
        der_neg[i] <- exp(-dist_neg[i])/denom
      }

      # update theta[pos[1], ], theta[pos[2], ].
      theta[pos[1], ] <- project(theta[pos[1], ] + lr  * (1 - as.numeric(crossprod(theta[pos[1], ])))^2 / 4 * der_p * getDistanceGradVec(theta[pos[1], ], theta[pos[2], ]))
      theta[pos[2], ] <- project(theta[pos[2], ] + lr  * (1 - as.numeric(crossprod(theta[pos[2], ])))^2 / 4 * der_p * getDistanceGradVec(theta[pos[2], ], theta[pos[1], ]))

      # update theta[pos[1], ], theta[neg[j], ], j = 1, 2, ..., n_neg.
      for (i in 1:n_neg) {
        theta[neg_mat[, i][1], ] <- project(theta[neg_mat[, i][1], ] + lr * (1 - as.numeric(crossprod(theta[neg_mat[, i][1], ])))^2 / 4 * der_neg[i] * getDistanceGradVec(theta[neg_mat[, i][1], ], theta[neg_mat[, i][2], ]))
        theta[neg_mat[, i][2], ] <- project(theta[neg_mat[, i][2], ] + lr * (1 - as.numeric(crossprod(theta[neg_mat[, i][2], ])))^2 / 4 * der_neg[i] * getDistanceGradVec(theta[neg_mat[, i][2], ], theta[neg_mat[, i][1], ]))
      }
    }
  }

  return(theta)
}
