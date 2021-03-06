setwd("~/Google Drive/PhD 2014/2014 R Research/Ebola")

### Required Libraries ###
library(maps)
library(mapproj)
library(scales)
library(mapplots)
library(animation)
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

# change NA to zero in cases and deaths
ebola$Cases[is.na(ebola$Cases) == T] = 0 
ebola$Deaths[is.na(ebola$Deaths) == T] = 0 

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
points(jitter(ebola.expanded$Long,30), jitter(ebola.expanded$Lat,30), col= case.color , cex=.5, pch=16)

legend('topleft', 
       legend = "1 Case", 
       pch = 16,
       pt.cex = .5,
       col = "red"       
)

# map relevant capitals #
text(x = capitals$Longitude, y = capitals$Latitude, labels = capitals$Capital, cex=.75 )


###### add opacity color scale  #######


# zoomed in map # 
map.input <- c("Togo", "Benin", "Nigeria", "Niger", "Burkina Faso", 
               "Ghana", "Ivory Coast", "Liberia", "Sierra Leone", "Guinea", "Mali")
map('world', region=map.input, col="black") 

# jittered points #
points(jitter(ebola.expanded$Long,30), jitter(ebola.expanded$Lat,30), col= alpha(case.color, 0.5) , cex=.5, pch=16)

legend('topleft', 
       legend = "1 Case", 
       pch = 16,
       pt.cex = .5,
       col = "red"       
)

# map relevant capitals #
text(x = capitals$Longitude, y = capitals$Latitude, labels = capitals$Capital, cex=.75 )




######## pie charts on map  ##########

map.input <- c("Togo", "Benin", "Nigeria", "Niger", "Burkina Faso", 
               "Ghana", "Ivory Coast", "Liberia", "Sierra Leone", "Guinea", "Mali", "Bissau",
               "Senegal", "Mauritania","Banjul")
map('world', region=map.input, col="black") 


# set colors #
plot.colors = c(alpha("orange", 0.6), alpha("blue", 0.6))

legend('topleft', 
       title = "Proportion of Deaths",
       legend = c("Not Deaths", "Deaths"), 
       col= plot.colors,
       pch = c(22,22),
       pt.bg = plot.colors,
       inset = c(-.3,0)
)




for(i in 1:nrow(ebola)) {
  total = ebola$Cases + ebola$Deaths
  props = data.frame( ebola$Cases/(total), ebola$Deaths/total)
  
  if(is.na(props[,1][i]) == F) {
    
   
    add.pie(z= c(props[,1][i], props[,2][i]), x=ebola$Long[i], 
              y=ebola$Lat[i], radius= .35, 
              col=plot.colors, labels="") 
                      }

}

# map relevant capitals #
text(x = capitals$Longitude-2.5, y = capitals$Latitude, labels = capitals$Capital, cex=.75 )



## view the data over time ###### 

# unique takes # 
dates = unique(ebola$Date)

#generate data set
ebola.expanded.dates <-  ebola[rep(row.names(ebola), ebola$Cases), c(3,4,9)]  

#save gif! #
saveGIF({
for(i in 1:length(dates)) {
  
ebola.sub = ebola.expanded.dates[which(ebola.expanded.dates$Date == dates[i]),]  

map.input <- c("Togo", "Benin", "Nigeria", "Niger", "Burkina Faso", 
               "Ghana", "Ivory Coast", "Liberia", "Sierra Leone", "Guinea", "Mali")
map('world', region=map.input, col="black") 
title(main=dates[i])

# jittered points #
points(jitter(ebola.sub$Long,3), jitter(ebola.sub$Lat,3), col= alpha(case.color, 0.5) , cex=.5, pch=16)

legend('topleft', 
       legend = "1 Case", 
       pch = 16,
       pt.cex = .5,
       col = "red"       
)

# map relevant capitals #
text(x = capitals$Longitude, y = capitals$Latitude, labels = capitals$Capital, cex=.75 )


}

})





