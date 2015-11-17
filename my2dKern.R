# Varying bandwidth kernel smothing 

x <- c(0, 1, 1.1, 1.5, 1.9, 2.8, 2.9, 3.5)
y <- c(0, 1, 1.1, 1.5, 1.9, 2.8, 2.9, 3.5)

my2dKern <- function(x, y, h, nbins){
  xgrid <- seq(from = min(x), to = max(x), length.out = nbins)
  ygrid <- seq(from = min(y), to = max(y), length.out = nbins)
  nx <- length(x)
  kd <- c()
  l <- 1
  for(i in 1:length(xgrid)){
    xval <- xgrid[i] 
    for(j in 1:length(ygrid)){
      yval <- ygrid[j] 
      value <- c()
        for(k in 1:length(x)){
          xx <- xval - x[k]
          yy <- yval - y[k]
          value[k] <- ((1.0 / (2.0*pi*h[k]*h[k])) * exp(-1.0 * (xx*xx + yy*yy) / (2.0*h[k]*h[k])))/(nx);
        }
      kd[l] <- sum(value)
      l <- l + 1
    }
  }
  kds <- matrix(kd, ncol = nbins, nrow = nbins, byrow = T)
  return(kds)
}
b <- my2dKern(x = x, y = y, h = rep(3, length(x)), nbins = 10)
z <- kde2d(x = x, y = y, h = c(12,12), n = 10) 
all.equal(b, z$z)



