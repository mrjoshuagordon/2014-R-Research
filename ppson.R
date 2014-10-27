install.packages("spatstat")
library(spatstat)
require(maps)

lambda = 0.1 # intensity of the process
lon = c(-100,-70) # domain's longitude
lat = c(-40,10)   # domain's latitude

sim = rpoispp(lambda, win=c(lon,lat))
dev.off()
# do the plot
par(mar=c(1,1,1,1))
map("world", xlim=lon, ylim=lat, fill=TRUE)
map.axes() # add axes
plot(sim, chars=19, cols="red",cex=0.5, add=TRUE)

# add other process

lon1 = c(-95,-85) # other area
lat1 = c(-5,5)
sim1 = rpoispp(5*lambda, win=c(lon1,lat1))
plot(sim1, chars=19, cols="blue",cex=0.5, add=TRUE)