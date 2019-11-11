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

  return(distanceMtx) # N by N distance matrix with ij element = d(theta_i, theta_j)
}


# projection function
proj <- function(theta_i) { #input : should be one row(vector) of theta matrix

  return(theta_i)
}


#get Euclidean Gradient w.r.t theta
getDistanceGradVec <- function(theta_i, theta_j) {

  return(distanceGradVec)
}

# embedding trainer -> output : trained theta.
embeddingTrainer <- function(POS, NEG, entity, theta_dim=2, N_epoch=100, lr=0.2, n_neg=4){


  return(theta)
}
