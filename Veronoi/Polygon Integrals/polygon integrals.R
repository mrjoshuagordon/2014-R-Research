
library(polyCub)



### Short comparison of the various cubature methods
## 2D-function to integrate (here: isotropic zero-mean Gaussian density)

## simple polygonal integration domain
disc.owin <- spatstat::disc(radius=5, centre=c(3,2), npoly=3)



class(disc.owin)


p2.x.long <- c(4,4,8,8,6)
p2.y.lat <- c(2,4,4,2,1)
p2.length <- length(p2.x.long)
p2 <- data.frame(PID=rep(2, p2.length), POS=1:p2.length, X= p2.x.long , Y= p2.y.lat )
plot(1,1,ylim=c(0,5),xlim=c(0,9), t="n", xlab="", ylab="")
polygon(p2$X, p2$Y)

#need to reverse 
owin1 <-  owin(poly= data.frame( x = rev(p2$X) ,y = rev(p2$Y)) )

## plot image of the function and integration domain




# Integral 
f <- function (x=x,y=1){ 
100 * x^2 * abs(y)  
  } 

plotpolyf(owin1, f, xlim=c(-8,8), ylim=c(-8,8))

polyCub(owin1, f, method = c("exact.Gauss"), plot = T)


