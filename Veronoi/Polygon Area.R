# find the area of intersecting polygons

install.packages('PBSmapping');install.packages('splancs')

library(PBSmapping)
library(splancs)

# grid
p1.x.long <- floor(10*runif(4))
p1.y.lat  <- c(1,2,2,1)
p1.length <- length(p1.x.long) 

# voronoi 
p2.x.long <- c(4,4,8,8,6)
p2.y.lat <- c(2,4,4,2,1)
p2.length <- length(p2.x.long)

# converting to poly format
p1 <- data.frame(PID=rep(1, p1.length), POS=1:p1.length, X=p1.x.long, Y=p1.y.lat)
p2 <- data.frame(PID=rep(2, p2.length), POS=1:p2.length, X= p2.x.long , Y= p2.y.lat )
p3 <- joinPolys(p1,p2)

par(mar=c(3,3,1,1))
plot(1,1,ylim=c(0,5),xlim=c(0,9), t="n", xlab="", ylab="")
polygon(p1$X, p1$Y, border=2)


polygon(p3$X, p3$Y, col='blue')

m <- cbind(p3$X, p3$Y)
areapl(m)