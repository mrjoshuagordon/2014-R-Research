# libraries 
library(PBSmapping)
library(splancs)
library(animation)
library(scales)

#generate empty grid 
x <- seq(0,5, by = 1)
y <- seq(0,5, by = 1)
xy <- expand.grid(x,y)
plot(xy, main="", xlab="", ylab="")
grid(NULL, NULL, lty=6) 

#generate pixel coordinates for polygones ############################ 
x1 = 0 
y1 = 0
x2 = 1
y2 = 1
inits = c(x1,y1,x1,y2,x2,y2,x2,y1)
id = data.frame(t(inits))

count <- 1
j <- 1
while(j <= 5) {

for(i in 0:4){
  y1 <- i ; y2 <- i + 1
  inits = c(x1,y1,x1,y2,x2,y2,x2,y1)
  id[count,] = inits 
  count <- count + 1
  i <- i + 1
  print(inits)
}
  y1 <- 0; y2 <- 1;
  x1 <- x1 + 1; x2 <- x2 + 1;

  j <- j + 1
}

####################################################################


# some random conditional intensitites ############################## 
rl <- floor(runif(nrow(id))*25)+1 


## Calculate Area with Lambda Weights for Each grid and save to a gif 
total.lambda = c()
counter <- 1
p3.list <- list()

saveGIF({
for(i in 1:nrow(id)) {  

# overlay voronoi
p2.x.long <- c(1.3,1.5,2.1,3.5,4.9)
p2.y.lat <- c(1.1,3.5,3.6,2,1.4)
p2.length <- length(p2.x.long)
p2 <- data.frame(PID=rep(2, p2.length), POS=1:p2.length, X= p2.x.long , Y= p2.y.lat ) 

#grid
p1.x.long <-  as.numeric(id[i,][c(1,3,5,7)])
p1.y.lat  <-  as.numeric(id[i,][c(2,4,6,8)])
p1.length <- length(p1.x.long) 
p1 <- data.frame(PID=rep(1, p1.length), POS=1:p1.length, X=p1.x.long, Y=p1.y.lat)

# overlapping area
p3 <- joinPolys(p1,p2)

# check if worth plotting or continuing
if(!is.null(p3)) {
  
#generate plot 
plot(xy, main="", xlab="", ylab="")
grid(NULL, NULL, lty=6)   
  
# voronoi
polygon(p2$X, p2$Y, border="red", lty="dashed", lwd=2)
  
#grid
polygon(p1.x.long, p1.y.lat, border="orange", lty="dashed", lwd=2)
  
  # confirm pixel coorindates are correct and plot lambdas for visualization
  for(k in 1:nrow(id)){
    xs <- as.numeric(id[k,][c(1,3,5,7)])
    ys <- as.numeric(id[k,][c(2,4,6,8)])
    text(mean(xs), mean(ys), labels=expression(paste(lambda, " = ")))
    text(mean(xs)+.2, mean(ys), labels= as.character(rl[k]))
    }  
  
p3.list[[counter]] <- p3 
  
# replot old coordinates  
for(j in 1:counter) {  
  polygon(p3.list[[j]]$X, p3.list[[j]]$Y, col=alpha('blue',.4)) 
}  
  

# area calculations
m <- cbind(p3$X, p3$Y)
am <- areapl(m)

# area times lambda
total.lambda = c(total.lambda, am*rl[i])
title( main = bquote(sum(lambda[i]) == .(round(sum(total.lambda),3))))

counter <- counter + 1
} # end if



}

})