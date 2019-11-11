# define poincare distance between two vectors.
getPoincareDistanceVec <- function(theta_i, theta_j) { # input : two vectors
  STABILITY <- 1e-4
  alpha <- 1 - as.numeric(crossprod(theta_i))
  beta <- 1 - as.numeric(crossprod(theta_j))
  gamma <- 1 + 2 / (alpha * beta + STABILITY) * as.numeric(crossprod(theta_i - theta_j))
  distance <- acosh(gamma)

  return(distance) # d(theta_i, theta_j)
}

# define poincare distance in matrix.
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
proj <- function(theta_i) { #input : should be one row(vector) of theta matrix

  epsilon <- 1e-5

  if (as.numeric(crossprod(theta_i)) >= 1) {
    theta_i <- theta_i / sqrt(as.numeric(crossprod(theta_i))) - epsilon
  }

  return(theta_i)
}


#get Euclidean Gradient w.r.t theta
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
embeddingTrainer <- function(POS, NEG, entity, theta_dim=2, N_epoch=100, lr=0.2, n_neg=4){


  return(theta)
}
