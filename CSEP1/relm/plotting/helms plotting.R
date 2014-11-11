
### Plotting Densities of Scaled Residuals and Raw Resduals ###

par(mfrow=c(2,1))
plot(density(scaled.res))
plot(density(raw.res))

# one sided plot from 0
plot(density(c(X, -X), from=0))

dev.off()
# r \sim 1 - X;  X \sim \Gamma(3.569,3.569)

plot(density(X), ylim=c(0,1))
plot(density(vers$summary$del.area^2/X), ylim=c(0,1))
x <- seq(from=0, to=10, length.out=100)
ylim <- c(0, 0.2)
lines(x, dgamma(x,shape=3.569,rate=3.569), main="Gamma",     # Plot a gamma density
      type='l', ylim=ylim, col="red")


# PIT transform 
p.val <- pgamma(X,shape=3.569,rate=3.569)

# # generate palette, blue is overprediction, red is underprediction
# reds <- colorRampPalette(c("red", "white"))(length(which(p.val<=.5)))
# blues <- colorRampPalette(c("white", "blue"))(length(which(p.val>.5)))
# cols <- c(reds,blues)[order(p.val)]

# generate palette, blue is overprediction, red is underprediction, cut palette
cp <- colorRampPalette(c("firebrick4", "red", "white", "blue", "dodgerblue"))(nrow(obs)) 
cols <- cp[cut(p.val, nrow(obs))]

 
# #cols.red <- alpha("red", seq(0, 1, length = 10))
# #cols.blue <- alpha("blue", seq(0, 1, length = 10))
# cols <- c(rev(alpha("red", seq(0, 1, length = nrow(obs)/2))), alpha("blue", seq(0, 1, length = nrow(obs)/2))) 
# cols <- cols[cut(p.val, nrow(obs))]


# color assessment 
df <- data.frame(round(X,2), round(p.val,2), cols)
df.sort <- df[order(df[,2], decreasing = T),]

## plotting with legend 
dev.off()

par(bg = "white") 

plot(vers, wpoints='non', wlines='tess', fill=alpha(cols,1))
map('state', 'california', add=T, lty = 2, lwd=2)

# observed points
points(obs$Longitude, obs$Latitude, pch=".", col="yellow")

#highlight a cell 
p2.x.long <- tl[[4]]$x
p2.y.lat <- tl[[4]]$y
p2.length <- length(p2.x.long)
p2 <- data.frame(PID=rep(2, p2.length), POS=1:p2.length, X= p2.x.long , Y= p2.y.lat ) 
polygon(p2$X, p2$Y, border="yellow", lty="dashed", lwd=2)

#legend from fields packages 
image.plot(legend.only=T, zlim=c(0,1), col=cols[order(p.val)], lwd=.1) 






