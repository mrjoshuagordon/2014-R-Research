
### Plotting Densities of Scaled Residuals and Raw Resduals ###

par(mfrow=c(2,1))
hist(scaled.res, prob=T, breaks=100)
lines(density(scaled.res, bw=1.4), col="red")
plot(density(raw.res))

# one sided plot from 0
plot(density(c(X, -X), from=0))

dev.off()
# r \sim 1 - X;  X \sim \Gamma(3.569,3.569)
plot(density(raw.res ), ylim=c(0,1), xlim = c(-3, 3), "Raw Residuals", xlab="Voronoi Residual")
#plot(density(vers$summary$del.area^2/X), ylim=c(0,1))
x <- seq(from=0, to=10, length.out=1000)
ylim <- c(0, 0.2)
lines(x, dgamma(x,shape=3.569,rate=3.569), main="Gamma",     # Plot a gamma density
      type='l', ylim=ylim, col="red")
legend('topleft', legend=c('Raw Residuals', 'Gamma(3.569,3.569)'), col=c('black','red'), lty=c(1,1))

residuals(raw.res)




# PIT transform 
p.val <- pgamma(X,shape=3.569,rate=3.569)
p.val <- qnorm(p.val)
p.val <- pgamma(scaled.res,shape=3.569,rate=3.569)
p.val <- scaled.res
#p.val <- qnorm(scaled.res)
# # generate palette, blue is overprediction, red is underprediction
# reds <- colorRampPalette(c("red", "white"))(length(which(p.val<=.5)))
# blues <- colorRampPalette(c("white", "blue"))(length(which(p.val>.5)))
# cols <- c(reds,blues)[order(p.val)]

# generate palette, blue is underprediction by helms model, red is overprediction by helms model, cut palette into equal parts (UGLY code)
cp <- colorRampPalette(c("firebrick4", "red", "white", "white",  "dodgerblue","blue"))(51)
#cp <- rev(cp)
vals <- data.frame(p.val[order(p.val)], 1:length(p.val))
p.vals <- data.frame(p.val, rep(0, length(p.val)))
p.vals[match(vals[,1], p.vals[,1]),2]  <- 1:nrow(p.vals)
p.vals[which(p.vals[,2] == 0),2] <- 510
names(p.vals)  <- c("p.val", "rank")
cols <- cp[cut(p.vals$rank, 51)]

dev.off()

# color assessment 
df <- data.frame(round(X,2), round(p.val,2), cols)
df.sort <- df[order(df[,2], decreasing = T),]

## plotting with legend 
dev.off()

par(bg = "white") 
par(oma=c( 0,0,0,1))# reset margin to be much smaller.
plot(vers, wpoints='non', wlines= 'tess', fill=alpha(cols,1),col= 'gray')
map('state', 'california', add=T, lty = 2, lwd=2)

# observed points
#points(obs$Longitude, obs$Latitude, pch=".", col="yellow")
map('state', 'california', lty = 2, lwd=2, xlim=c(xmin, xmax), ylim=c(ymin, ymax))
map.axes()
#highlight a cell 
for(i in 1:length(tl)) {
p2.x.long <- tl[[i]]$x
p2.y.lat <- tl[[i]]$y
p2.length <- length(p2.x.long)
p2 <- data.frame(PID=rep(2, p2.length), POS=1:p2.length, X= p2.x.long , Y= p2.y.lat ) 
polygon(p2$X, p2$Y, border="white", lty="solid", lwd=.5, col=cols[i])
}
map('state', 'california', add=T, lty = 2, lwd=2, col='slategray')

#legend from fields packages 
#image.plot(legend.only=T, zlim=c(0,1), col= cols[rev(order(p.val))], lwd=.1) 
#image.plot(legend.only=T, zlim=c(0,1), col= cols[order(p.vals$p.val)], lwd=.1, axis.args=list( at=c(0,.5,1), labels=c(0,.5,1) )) 

image.plot(legend.only=T, zlim=c(0,1), col= cols[rev(order(p.vals$p.val))], 
           lwd=.1, axis.args=list( at=c(0,.5,1), labels=c(0,.5,1) ))  


# # areas 
# mm <- head(order(vers$summary$del.area, decreasing=T)) 
# for(i in mm){
#  text(tl[[i]]$pt[1], tl[[i]]$pt[2], labels=round(X[i],3))
#   
# }


