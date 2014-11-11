#data
setwd('~/Google Drive/PhD 2014/2014 R Research/CSEP1/relm')
load("RELMdata.RData")

# Libraries 
lib <- c("spatstat", "deldir", "splancs", 'PBSmapping', 'scales')
lapply(lib, require, character.only=T)

#grid set up
fgrid <- unique(data.frame(ebel$'minimum longitude',ebel$'max longitude', ebel$'min latitude', ebel$'max latitude'))
names(fgrid) <- c('min.long','max.long', 'min.lat', 'max.lat')

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

# aggregate the rate over each grid cell 
helmsAgg <- aggregate(gridNumbers$rate,by=list(gridNumbers$gridNumber,
                                              gridNumbers$minimum.longitude,
                                              gridNumbers$max.longitude,
                                              gridNumbers$min.latitude,
                                              gridNumbers$max.latitude                                              
),FUN=sum)


names(helmsAgg) <- names(gridNumbers)
helmsAgg <- helmsAgg[order(helmsAgg[,1]),]
head(helmsAgg)


#veronoi cell setup 
vers <- deldir(obs$Longitude,  y=obs$Latitude)
tl <- tile.list(vers)


# viewing window
#plot(x=0, y=0, xlim=c(-117.5,-116.5), ylim=c(37,38), type='n', xlab="", ylab="")

map('state', 'california')
#plotting veronoi
for(i in 1:length(tl)){
  points(tl[[i]]$pt[1],tl[[i]]$pt[2], cex=1, col='red', pch=16)
  
  # overlay voronoi
  p2.x.long <- tl[[i]]$x
  p2.y.lat <- tl[[i]]$y
  p2.length <- length(p2.x.long)
  p2 <- data.frame(PID=rep(2, p2.length), POS=1:p2.length, X= p2.x.long , Y= p2.y.lat ) 
  polygon(p2$X, p2$Y, border="gray", lty="dashed", lwd=2)
  
  # Find which grid points to compare the cell to
  gridMatch <- which(helmsAgg$minimum.longitude >= min(p2.x.long)- .1 & 
                      helmsAgg$max.longitude    <= max(p2.x.long)+ .1 &
                      helmsAgg$min.latitude     >= min(p2.y.lat) - .1 &
                      helmsAgg$max.latitude     <= max(p2.y.lat) + .1 )

  
  #for(j in 5996){
  for(j in gridMatch) {
    
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
    polygon(p3$X, p3$Y, col='blue')
   
    #calculate area
    m <- cbind(p3$X, p3$Y)
    g <- cbind(p1$X, p1$Y)
    am <- (areapl(m)/areapl(g))*helmsAgg$rate[j]
        }

} # end gridMatch j loop

} # end tl Veronoi i loop


title(main=paste('Grid Cell Expectation: ', round(helmsAgg$rate[j], 6),
               '\nGrid within Tess Expecations: ' , round(am,6)))











# overlay grid on plot
for(i in 5996) {
#for(i in gridMatch) {
  
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
  text(x = mean(pixel.poly$long), y = mean(pixel.poly$lat),label=round(helmsAgg$rate[i],3), cex=.5)
  polygon(pixel.poly$long, pixel.poly$lat, border='gray90')
  
}




