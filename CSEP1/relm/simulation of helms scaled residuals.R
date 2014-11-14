head(helmsAgg)
hsim <- rpois( n = length(helmsAgg$rate) , lambda = helmsAgg$rate)

long.sim <- c()
lat.sim <- c()
for(i in 1:length(helmsAgg$rate)){
long.sim <-  c( long.sim, runif(n = hsim[i],helmsAgg$minimum.longitude[i], helmsAgg$max.longitude[i]))
lat.sim <- c( lat.sim, runif(n = hsim[i],helmsAgg$min.latitude[i], helmsAgg$max.latitude[i]))
}

sobs <- data.frame(Latitude = lat.sim, Longitude = long.sim )


# plot sims
map('state', 'california', lty = 2, lwd=1, xlim=c(xmin, xmax), ylim=c(ymin, ymax))
map.axes()
points(sobs$Longitude, sobs$Latitude, pch=16, col=alpha("black",.5))







findResiduals = function(obs, helmsAgg){

xmin <- min(helmsAgg$minimum.longitude)
xmax <- max(helmsAgg$max.longitude)
ymin <- min(helmsAgg$min.latitude)
ymax <- max(helmsAgg$max.latitude)
  
    
vers <- deldir(obs$Longitude,  y=obs$Latitude, rw=c(xmin, xmax, ymin, ymax))
tl <- tile.list(vers)
# viewing window
# store residuals 
X <- c()

raw.res <- c()
scaled.res <- c()
counter = c() 

for(i in 1:length(tl)){
  
  
  # overlay voronoi
  p2.x.long <- tl[[i]]$x
  p2.y.lat <- tl[[i]]$y
  p2.length <- length(p2.x.long)
  p2 <- data.frame(PID=rep(2, p2.length), POS=1:p2.length, X= p2.x.long , Y= p2.y.lat ) 
  
  
  # Find which grid points to compare the cell to
  gridMatch <- which(helmsAgg$minimum.longitude >= min(p2.x.long) - .1 & 
                       helmsAgg$max.longitude    <= max(p2.x.long)+ .1 &
                       helmsAgg$min.latitude     >= min(p2.y.lat) - .1 &
                       helmsAgg$max.latitude     <= max(p2.y.lat) + .1 )
  
  #initiate area vector
  am <- c()
  
  for(j in gridMatch) {
    counter = c(counter,j)
    #get the coordinates of the grid
    p1.x.long <-  helmsAgg[j,c(2,3)]
    p1.y.lat  <-  helmsAgg[j,c(4,5)]
    pixel <- expand.grid(as.numeric(p1.x.long), as.numeric(p1.y.lat))
    names(pixel) <- c('long', 'lat')
    
    # reorder into polygon format 
    pixel.poly <- data.frame(
      c(min(pixel$long),min(pixel$long), max(pixel$long),max(pixel$long)),
      c(min(pixel$lat), max(pixel$lat), max(pixel$lat),min(pixel$lat)))
    
    names(pixel.poly) = names(pixel)
    
    # generate polygone object for comparison
    p1.length <- length(pixel.poly$long) 
    p1 <- data.frame(PID=rep(1, p1.length), POS=1:p1.length, X=pixel.poly$long, Y=pixel.poly$lat)
    
    # compare the polygons, if there is actually overlap 
    p3 <- joinPolys(p1,p2)
    if(!is.null(p3)) {
      
      #calculate area
      
      #overlap area
      m <- cbind(p3$X, p3$Y)
      
      #grid cell area
      g <- cbind(p1$X, p1$Y)
      
      #store overlap area wieghted by proprition of grid in the area
      am <- c(am, (areapl(m)/areapl(g))*helmsAgg$rate[j])
    }
    
  } # end gridMatch j loop
  X[i] <- sum(am)
  scaled.res[i] <- (1-sum(am))/sqrt(sum(am))
  raw.res[i] <- 1 - sum(am)
  
  if(i %% 50 == 0 ) print(i)
} # end tl Veronoi i loop

return(scaled.res)

} # end findResiduals function 


#function to simulate data from the model 
generateObs <- function(helmsAgg){
  hsim <- rpois( n = length(helmsAgg$rate) , lambda = helmsAgg$rate)
  
  long.sim <- c()
  lat.sim <- c()
  for(i in 1:length(helmsAgg$rate)){
    long.sim <-  c( long.sim, runif(n = hsim[i],helmsAgg$minimum.longitude[i], helmsAgg$max.longitude[i]))
    lat.sim <- c( lat.sim, runif(n = hsim[i],helmsAgg$min.latitude[i], helmsAgg$max.latitude[i]))
  }
  
  sobs <- data.frame(Latitude = lat.sim, Longitude = long.sim )
  
  return(sobs)
  
}


# function to run the entire sim
simH <- function(helmsAgg){
sobs <- generateObs (helmsAgg = helmsAgg)
sc.sim1 <- findResiduals(obs= sobs, helmsAgg = helmsAgg)
}

a <- Sys.time()
# repeat the sim
sm1 <- replicate(100, simH(helmsAgg = helmsAgg))

save(sm1, file = 'sim1helms.RData')

hist(unlist(sm1), prob=T, breaks=100, main=length(unlist(sm1)))
lines(density(sc.sim1, bw=1.4), col="red")
b <- Sys.time()
b - a

par(mfrow=c(2,1))

hist(scaled.res, prob=T, xlim=c(-2,6), breaks=100, main="Scaled Residuals for Observed")
lines(density(scaled.res, bw=1.4), col="red")


hist(unlist(sm1), prob=T, xlim=c(-2,6), breaks=100, main="Scaled Residuals for 100 Simulations")
lines(density(unlist(sm1)), col="red") 

