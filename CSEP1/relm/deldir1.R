setwd('~/Google Drive/PhD 2014/2014 R Research/CSEP1/relm')
load("RELMdata.RData")

# Libraries 
lib <- c("spatstat", "deldir", "maps")
lapply(lib, require, character.only=T)

# Load Data: RELM Collection Region 2006-01-01 to 2014-01-01
# RELM
obs <- read.csv("relm_obs.csv")

#US Cities 
data(us.cities)

# Generate RELM Grid
grid <- unique(data.frame(ebel$'minimum longitude', ebel$'min latitude'))
names(grid) = c('long','lat')

## All of California ##
# Map Data
map('state', 'california')

# RELM Quakes 
points(x=obs$Longitude, y=obs$Latitude, pch='.', col='red', cex=2)

#spatstat
plot(deldir(obs$Longitude,  y=obs$Latitude), wlines=c('te'), wpoints=c('none'), col='slategray4', add=T)


## Los Angeles Area ##
long <- c(-119.25,-117)
lat <- c(33,35) 
map('county', xlim = long , ylim = lat , bg="gray97") 
map.cities(us.cities[us.cities$pop>200000,], cex=.5)
map.axes()

# RELM Quakes 
points(x=obs$Longitude, y=obs$Latitude, pch='.', col='red', cex=3)

#spatstat
plot(deldir(obs$Longitude,  y=obs$Latitude), wlines=c('te'), wpoints=c('none'), col='slategray3')

#grid
points(grid, pch='.',cex=2)










 
