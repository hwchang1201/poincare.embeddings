# library(ggplot2)

#plot the illustration of relation. This is tentative!!

#' A 2-dimension plot for the poincare embeddings.
#'
#' @param theta - A N x 2 matrix. Each of the column corresponds to the entity subject to embed.
#' @param nameOfSample - Name of the entity subject to embed.
#' @param POS - A 2-row positive relation matrix that contains positive relation entities columnwisely.
#' @param N_epoch - The number of epochs.
#' @param lr - The learning rate.
#' @param n_neg - The number of negative samples for each iteration.
#'
#' @return A 2-dimension plot with entity names and line segments between the entities with positive relation.
#' @export
#'
#' @examples toy_yaml <- yaml::yaml.load(toy)
#' @examples toy_tree <- data.tree::as.Node(toy_yaml)
#' @examples toy_dataset <- dataLoader(toy_tree)
#' @examples emb <- poincareEmbeddings(toy_tree, 2, 100, 0.001, 5)
#' @examples make2dPlot(emb$theta, toy_dataset$entity, toy_dataset$POS, 100, 0.001, 5)
#'
make2dPlot <- function(theta, nameOfSample, POS, N_epoch, lr, n_neg){
  #exception for dimension x(it should be two-dimensional)
  N <- dim(theta)[1]
  #plot the unit circle
  tt <- seq(0,2*pi,length.out = 100)
  xx <- cos(tt)
  yy <- sin(tt)
  #nameOfSample <- paste(1:N)

  plot(xx,yy,type = "l",xlim = c(-1,1),ylim = c(-1,1),xlab = '',ylab = '',
       main=paste("2D embedding plot for N_epoch :",  N_epoch, ", lr :",lr,", n_neg :", n_neg, sep=" "))
  par(new = TRUE)
  #plot the data points
  plot(theta[ , 1],theta[ , 2],xlim = c(-1,1),ylim = c(-1,1),xlab = '',ylab = '')
  text(theta[ , 1],theta[ , 2], labels = nameOfSample, cex = 0.7, pos = 3)
  segments(theta[, 1][POS[1, ]], theta[ , 2][POS[1, ]], theta[, 1][POS[2, ]], theta[ , 2][POS[2, ]])

}
