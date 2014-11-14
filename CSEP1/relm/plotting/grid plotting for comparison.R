


# gp <- colorRampPalette(c("blue", "white", "orange", "yellow", "red"))(51)
# gp <- substr(color.scale(helmsAgg$rate,,c(0,1,1),c(1,1,0),0, extremes=NA),0,7)  


gp <- colorRampPalette(c( "white", "orange", "yellow", "red", "darkred"))(20)
bins <- cut(helmsAgg$rate,quantile(helmsAgg$rate, seq(0,1,by=.05)))
bint <- as.data.frame(table(bins), gp)
binc <- rep(0, length(helmsAgg$rate))

for(i in 1:length(binc)){
  if(i != 4646) {
    binc[i] <- row.names(bint[which(bint$bins == bins[i]),])
  }
}

binc[4646] = "#FFFFFF"


map('state', 'california', lty = 2, lwd=.025, xlim=c(xmin, xmax), ylim=c(ymin, ymax))
map.axes()



# overlay grid on plot
#for(i in 1:300) {
for(i in 1:nrow(helmsAgg)) {

  
  p1.x.long <-  helmsAgg[i,c(2,3)]
  p1.y.lat  <-  helmsAgg[i,c(4,5)]
  
  pixel <- expand.grid(as.numeric(p1.x.long), as.numeric(p1.y.lat))
  names(pixel) = c('long', 'lat')
  
  # reorder into polygon format 
  pixel.poly = data.frame(
    c(min(pixel$long),min(pixel$long), max(pixel$long),max(pixel$long)),
    c(min(pixel$lat), max(pixel$lat), max(pixel$lat),min(pixel$lat)))
  
  names(pixel.poly) = names(pixel)
  
  #points(pixel.poly$long, pixel.poly$lat)
 # text(x = mean(pixel.poly$long), y = mean(pixel.poly$lat),label=round(helmsAgg$rate[i],3), cex=.5)
  polygon(pixel.poly$long, pixel.poly$lat, border='black', col = binc[i], lwd=.01)
  
}

#points(obs$Longitude, obs$Latitude, pch=".", col="yellow")
image.plot(legend.only=T, zlim=c(0,1), col= gp, 
           lwd=.1, axis.args=list( at=c(0,1), labels=c(0,10) )) 
 






# Plotting the observed
map('state', 'california', lty = 2, lwd=1, xlim=c(xmin, xmax), ylim=c(ymin, ymax))
map.axes()
points(obs$Longitude, obs$Latitude, pch=16, col=alpha("black",.5))




gres <- c()
# Grid Residuals 
for(i in 1:nrow(fgrid)){
  p1.x.long <-  fgrid[i,c(1,2)]
  p1.y.lat  <-  fgrid[i,c(3,4)]
  
  pixel <- expand.grid(as.numeric(p1.x.long), as.numeric(p1.y.lat))
  names(pixel) <- c('long', 'lat')
  
  # reorder into polygon format 
  pixel.poly <- data.frame(
    c(min(pixel$long),min(pixel$long), max(pixel$long),max(pixel$long)),
    c(min(pixel$lat), max(pixel$lat), max(pixel$lat),min(pixel$lat)))
  
  names(pixel.poly) = names(pixel)
  
  le <- length(which(obs$Longitude >= min(pixel.poly$long) &
          obs$Longitude <  max(pixel.poly$long) &
          obs$Latitude >= min(pixel.poly$lat) &
          obs$Latitude <  max(pixel.poly$lat) ))
  
  # if(length(le) == 0) {le = 0}
  
  gres[i] <- le
  
  
}

# estimate of grid residuals vs helms [obs - expected]
pxres <- gres - helmsAgg$rate 



# plot grid residuals # #################################################

# generate colors for grid residuals 
gp <- colorRampPalette(c( "blue", "white", "orange", "yellow", "red", "darkred"))(20)
bins <- cut(pxres,quantile(pxres, seq(0,1,by=.05)))
bint <- as.data.frame(table(bins), gp)
binc <- rep(0, length(pxres))

for(i in 1:length(binc)){
  if(length(row.names(bint[which(bint$bins == bins[i]),]))>0){
    binc[i] <- row.names(bint[which(bint$bins == bins[i]),])
  } else{
    print(i)
    
  }
}

binc[4757] = "#0000FF"






map('state', 'california', lty = 2, lwd=.025, xlim=c(xmin, xmax), ylim=c(ymin, ymax))
map.axes()

# overlay grid on plot
#for(i in 1:300) {
for(i in 1:nrow(helmsAgg)) {
  
  
  p1.x.long <-  helmsAgg[i,c(2,3)]
  p1.y.lat  <-  helmsAgg[i,c(4,5)]
  
  pixel <- expand.grid(as.numeric(p1.x.long), as.numeric(p1.y.lat))
  names(pixel) = c('long', 'lat')
  
  # reorder into polygon format 
  pixel.poly = data.frame(
    c(min(pixel$long),min(pixel$long), max(pixel$long),max(pixel$long)),
    c(min(pixel$lat), max(pixel$lat), max(pixel$lat),min(pixel$lat)))
  
  names(pixel.poly) = names(pixel)
  
  #points(pixel.poly$long, pixel.poly$lat)
  # text(x = mean(pixel.poly$long), y = mean(pixel.poly$lat),label=round(helmsAgg$rate[i],3), cex=.5)
  polygon(pixel.poly$long, pixel.poly$lat, border='black', col = binc[i], lwd=.01)
  
}

#points(obs$Longitude, obs$Latitude, pch=".", col="yellow")
image.plot(legend.only=T, zlim=c(0,1), col= gp, 
           lwd=.1, axis.args=list( at=c(0,1), labels=c(round(min(pxres,1)),max(pxres,1)) )) 


