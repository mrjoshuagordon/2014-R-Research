setwd("~/Google Drive/PhD 2014/2014 R Research/Ebola")

### Required Libraries ###
library(maps)
library(mapproj)

# cases # 
case.color = "red"

### Data Prep ###

# read in ebola data 
ebola <- read.csv("ebola_who.csv")

# relevant capitals
capitals <- data.frame(Capital = c("Porto-Novo", "Conakry", "Monrovia", "Bamako","Freetown"), 
                       Latitude = c(6.50,9.54,6.30,12.65,8.48),
                       Longitude = c(2.60,-13.68,-10.80,-8.00, -13.23)
)

# cast date column to date format 
ebola$Date <- as.Date(ebola$Date, "%m-%d-%Y")

# change NA to zero in cases
ebola$Cases[is.na(ebola$Cases) == T] = 0 

# create a scale based on the relative number of cases  ###
point.scale <- ceiling(ebola$Cases / 100)    
        
### Maps  ### 

## Africa Map with All Cases on Point Scale, No Legend  ##
long <- c(-25,60)
lat <- c(-35,37)
map("world",xlim=long,ylim=lat)
points(ebola$Long, ebola$Lat, col=case.color, cex=point.scale/6, pch=16)


## Regional Map with Size Proportional to Number of Caes ## 

# define plot area #
par(mar=c(8.1, 8.1, 4.1, 8.1), xpd=TRUE)

# regional map #
map.input <- c("Togo", "Benin", "Nigeria", "Niger", "Burkina Faso", 
            "Ghana", "Ivory Coast", "Liberia", "Sierra Leone", "Guinea", "Mali", "Bissau",
            "Senegal", "Mauritania","Banjul")
map('world', region=map.input, col="black") 

# scaled points #
points(ebola$Long, ebola$Lat, col=case.color, cex=1+point.scale/7, pch=16)

# point size and legend variables #
bins <- length(table(point.scale * 100))  
cexs <- as.numeric(names(table(1+point.scale/7)))
legend('topleft', 
       title = "Cases",
       legend = as.character(names(table(point.scale  * 100))), 
       pch = rep(x = 16, bins),
       pt.cex = cexs,
       col = rep(x = "red", length(cexs)),
       inset = c(-.2,0)
       )

# map relevant capitals #
text(x = capitals$Longitude, y = capitals$Latitude, labels = capitals$Capital, cex=.75 )

## One Dot per cases with Jitter#

# restucture the data for 1 point per cases # 
ebola.expanded <-  ebola[rep(row.names(ebola), ebola$Cases), 3:4] 

# zoomed in map # 
map.input <- c("Togo", "Benin", "Nigeria", "Niger", "Burkina Faso", 
               "Ghana", "Ivory Coast", "Liberia", "Sierra Leone", "Guinea", "Mali")
map('world', region=map.input, col="black") 

# jittered points #
points(jitter(ebola.expanded$Long,30), jitter(ebola.expanded$Lat,30), col=case.color, cex=.5, pch=16)

legend('topleft', 
       legend = "1 Case", 
       pch = 16,
       pt.cex = .5,
       col = "red"       
)

# map relevant capitals #
text(x = capitals$Longitude, y = capitals$Latitude, labels = capitals$Capital, cex=.75 )
