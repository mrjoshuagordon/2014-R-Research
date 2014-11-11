which((1:nrow(helmsAgg) %in%  unique(kk) ) == F)[1] 

out <- 32
helmsAgg[out,]



long <- c(min(helmsAgg$minimum.longitude),max(helmsAgg$max.longitude))
lat <- c(min(helmsAgg$min.latitude),max(helmsAgg$max.latitude))  



p1.x.long <-  helmsAgg[out,c(2,3)]
p1.y.lat  <-  helmsAgg[out,c(4,5)]
pixel <- expand.grid(as.numeric(p1.x.long), as.numeric(p1.y.lat)) 

names(pixel) <- c('long', 'lat')



plot(deldir(obs495$Longitude,  y=obs495$Latitude, rw = c(long[1], long[2], lat[1], lat[2])), wlines=c('te'), wpoints=c('none') )
points(pixel$long, pixel$lat, pch=16, col='red', cex=.5)
map('state', 'california',xlim = long , ylim = lat, add=T )



cat = read.table('cat090214.dat')


obs$Magnitude
cat$V6


