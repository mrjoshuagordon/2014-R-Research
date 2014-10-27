install.packages("RColorBrewer")
library(datautils)
library(RColorBrewer)

X <- runifpoint(42)
plot(dirichlet(X))
plot(X, add=TRUE)


x = matrix()

#spatstat
plot(deldir(pp1$x, pp1$y), wlines=c('tess'), fill=colorRampPalette(brewer.pal(9,"Blues"))(length(pp1$x)) )

#josh's
plot(deldir(nv[,1], nv[,2]), wlines=c('tess'), fill=colorRampPalette(brewer.pal(9,"Greens"))(length(pp1$x)) )  


xvals <- rnorm(50)
yvals <- rnorm(50)
res <- deldir(xvals, yvals)

rvalues <- runif(50)
gvalues <- runif(50)
bvalues <- runif(50)
plot(res, wlines="tess", fill=rgb(rvalues, gvalues, bvalues))
