library(ggplot2)

#plot the illustration of relation. This is tentative!!

make2dPlot <- function(x, nameOfSample, POS, N_epoch, lr, n_neg){
  #exception for dimension x(it should be two-dimensional)
  N <- dim(x)[1]
  #plot the unit circle
  tt <- seq(0,2*pi,length.out = 100)
  xx <- cos(tt)
  yy <- sin(tt)
  #nameOfSample <- paste(1:N)

  plot(xx,yy,type = "l",xlim = c(-1,1),ylim = c(-1,1),xlab = '',ylab = '',
       main=paste("2D embedding plot for N_epoch :",  N_epoch, ", lr :",lr,", n_neg :", n_neg, sep=" "))
  par(new = TRUE)
  #plot the data points
  plot(x[ , 1],x[ , 2],xlim = c(-1,1),ylim = c(-1,1),xlab = '',ylab = '')
  text(x[ , 1],x[ , 2], labels = nameOfSample, cex = 0.7, pos = 3)
  segments(x[, 1][POS[1, ]], x[ , 2][POS[1, ]], x[, 1][POS[2, ]], x[ , 2][POS[2, ]])

}
