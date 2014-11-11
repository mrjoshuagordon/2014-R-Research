#data
setwd('~/Google Drive/PhD 2014/2014 R Research/CSEP1/relm')
load("RELMdata.RData")


# Libraries 
lib <- c('spatstat', 'deldir', 'splancs', 'PBSmapping', 'scales', 'datautils', 'RColorBrewer', 'maps', 'fields')
lapply(lib, require, character.only=T)

#options
options(digits=9)

#grid set up
fgrid <- unique(data.frame(ebel$'minimum longitude',ebel$'max longitude', ebel$'min latitude', ebel$'max latitude'))
names(fgrid) <- c('min.long','max.long', 'min.lat', 'max.lat')

coords <- paste(helms[,1], helms[,2], helms[,3], helms[,4], sep="")
fgrd <- paste(fgrid[,1],fgrid[,2], fgrid[,3], fgrid[,4], sep="")
gridNumbers <- match(coords,fgrd)

gridNumbers <- data.frame(gridNumber, helms[,c(1:4,9)])

# aggregate the rate over each grid cell 
helmsAgg <- aggregate(gridNumbers$rate,by=list(gridNumbers$gridNumber,
                                               gridNumbers$minimum.longitude,
                                               gridNumbers$max.longitude,
                                               gridNumbers$min.latitude,
                                               gridNumbers$max.latitude                                              
),FUN=sum)




names(helmsAgg) <- names(gridNumbers)
helmsAgg <- helmsAgg[order(helmsAgg[,1]),]
helmsAgg$rate <- helmsAgg$rate*1.75
head(helmsAgg)


#veronoi cell setup 

# set up min and max bounds for deldir
xmin <- min(helmsAgg$minimum.longitude)
xmax <- max(helmsAgg$max.longitude)
ymin <- min(helmsAgg$min.latitude)
ymax <- max(helmsAgg$max.latitude)

# subset the data to 4.95 and higher/ run deldir / get tilelist

vers <- deldir(obs$Longitude,  y=obs$Latitude, rw=c(xmin, xmax, ymin, ymax))
tl <- tile.list(vers)



# viewing window
#plot(x=0, y=0, xlim=c(-117.5,-116.5), ylim=c(37,38), type='n', xlab="", ylab="")

# store residuals 
X <- c()
scaled.res <- c()
raw.res <- c()

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
     # polygon(p3$X, p3$Y, col='blue')
      
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
  
  if(i %% 5 == 0 ) print(i)
} # end tl Veronoi i loop

# checking 
sum(X) / sum(helmsAgg$rate)


length(unique(counter)) 
nrow(helmsAgg)

