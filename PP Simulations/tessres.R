install.packages("stppResid")
library(stppResid)

#===> load simulated data <===#
data(simdata)
X <- stpp(simdata$x, simdata$y, simdata$t)

#===> define two conditional intensity functions <===#
ci1 <- function(X, theta){theta*exp(-2*X$x - 2*X$y - 2*X$t)} #correct model

ci2 <- function(X, theta = NULL){rep(250, length(X$x))} #homogeneous Poisson model

## Not run: 
tsresiduals <- tessresid(X, ci1, theta = 3000)
tsresiduals2 <- tessresid(X, ci2)
#===> plot results <===#
plot(tsresiduals)
plot(tsresiduals2)



library(deldir)
library(datautils)
library(RColorBrewer) 

#Simulate Non Homogenous PP using spatstat

lambda.fun1 = function(x,y){200*(x^2)*abs(y)} 
pp1=rpoispp(lambda = lambda.fun1,win=owin(c(-1,1),c(-1,1)));

X <- stpp(pp1$x, pp1$y, t=runif(length(pp1$x)), stw <- stwin(xcoord = c(-1, 1), ycoord = c(-1, 1), tcoord = c(0, 1)))

#===> define two conditional intensity functions <===#
ci1 <- function(X, theta = NULL){200 * X$x^2 * abs(X$y)} #correct model

ci2 <- function(X, theta = NULL){rep(250, length(X$x))} #homogeneous Poisson model

## Not run: 
tsresiduals <- tessresid(X, ci1, theta = 3000)
#===> plot results <===#
plot(tsresiduals)

