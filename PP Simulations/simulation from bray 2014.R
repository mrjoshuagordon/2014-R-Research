library(spatstat)
library(deldir)

dev.off();
pp1<-rpoispp(lambda = function(x,y){200*(x^2)*abs(y)},win=owin(c(-1,1),c(-1,1)));
plot(pp1)

#summary(pp1)

Q = quadratcount(pp1, nx = 4, ny = 3)
plot(Q, add = TRUE, cex = 2)

dev.off()
#veronoi tesselation 
Z <- dirichlet(pp1)
plot(Z)
points(pp1, col="red", cex=.5)
#Note that lambda is the intensity, that is, the expected number of points per unit area.



Ipp.fit1<-ppm(Q= lambda)
summary(Ipp.fit1) 




model = function(x,y) { 200*(x^2)*abs(y) }
?ppois


x = seq(-1,1,by=.001)
y = seq(-1,1,by=.001)





# estimating the integral
# http://www1.maths.leeds.ac.uk/~voss/projects/2012-Poisson/Drazek.pdf
g <- function(y) {exp(-.5*y^2)}
L6=integrate(f=g, lower=0,upper=Inf)
L6$value



