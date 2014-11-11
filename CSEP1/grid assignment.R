fgrid <- unique(data.frame(ebel$'minimum longitude',ebel$'max longitude', ebel$'min latitude', ebel$'max latitude'))
names(fgrid) = c('min.long','max.long', 'min.lat', 'max.lat')


# overlapy a grid
map('state', 'california', add=T) 

for(i in 1:nrow(fgrid)){
  p1.x.long <-  fgrid[i,c(1,2)]
  p1.y.lat  <-  fgrid[i,c(3,4)]
  points( p1.x.long , p1.y.lat, pch='.' )
  
} 


# assigned each pixel a number
gridNumber = rep(0, nrow(helms))


coords = paste(helms[,1], helms[,2], helms[,3], helms[,4], sep="")

for(i in 1:nrow(fgrid)){
  fg = fgrid[i,]  
  fgs = paste(fg[1], fg[2], fg[3], fg[4], sep="")
  gridNumber[which(coords == fgs)] = i
  
  if(i %% 100 == 0) print(i)
  
}


gridNumbers = data.frame(gridNumber, helms[,c(1:4,9)])



helmsAgg = aggregate(gridNumbers$rate,by=list(gridNumbers$gridNumber,
                                              gridNumbers$minimum.longitude,
                                              gridNumbers$max.longitude,
                                              gridNumbers$min.latitude,
                                              gridNumbers$max.latitude                                              
                                              ),FUN=sum)


names(helmsAgg) = names(gridNumbers)
helmsAgg = helmsAgg[order(helmsAgg[,1]),]

head(helmsAgg)



#plotting
plot(x=0, y=0, xlim=c(-126,-124), ylim=c(39,42), type='n', xlab="", ylab="")

# RELM Quakes 
points(x=obs$Longitude, y=obs$Latitude, pch='.', col='red', cex=2)

for(i in 1:1000) {

p1.x.long <-  helmsAgg[i,c(2,3)]
p1.y.lat  <-  helmsAgg[i,c(4,5)]

pixel = expand.grid(as.numeric(p1.x.long), as.numeric(p1.y.lat))
names(pixel) = c('long', 'lat')

# reorder into polygon format 
pixel.poly = data.frame(
  c(min(pixel$long),min(pixel$long), max(pixel$long),max(pixel$long)),
  c(min(pixel$lat), max(pixel$lat), max(pixel$lat),min(pixel$lat)))

names(pixel.poly) = names(pixel)


#points(pixel.poly$long, pixel.poly$lat)
text(x = mean(pixel.poly$long), y = mean(pixel.poly$lat),label=round(helmsAgg$rate[i],3), cex=.5)
polygon(pixel.poly$long, pixel.poly$lat, border='gray90')

}



plot(deldir(obs$Longitude,  y=obs$Latitude), wlines=c('te'), wpoints=c('none'), col='slategray4', add=T)



