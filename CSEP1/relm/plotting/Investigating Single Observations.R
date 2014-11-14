map('state', 'california', lty = 2, lwd=2, xlim=c(min(tl[[112]]$x),min(tl[[112]]$x) + (max(tl[[112]]$y) - min(tl[[112]]$y))), ylim=c(min(tl[[112]]$y),max(tl[[112]]$y)))
points(obs$Longitude, obs$Latitude, pch=16, col=alpha("black",.5))

map('state', 'california', lty = 2, lwd=2, xlim=c(min(tl[[112]]$x)-.5,min(tl[[112]]$x) + .5 + (max(tl[[112]]$y) - min(tl[[112]]$y))), ylim=c(min(tl[[112]]$y)-.5,max(tl[[112]]$y)+.5))

map.axes()
#highlight a cell 
for(i in 1:length(tl)) {
  p2.x.long <- tl[[112]]$x
  p2.y.lat <- tl[[112]]$y
  p2.length <- length(p2.x.long)
  p2 <- data.frame(PID=rep(2, p2.length), POS=1:p2.length, X= p2.x.long , Y= p2.y.lat ) 
  polygon(p2$X, p2$Y, border="white", lty="solid", lwd=.5, col=cols[112])
}



 text(tl[[112]]$pt[1], tl[[112]]$pt[2] + .02, labels=round(scaled.res[112],3))
  




# now plot all
for(i in 1:length(tl)) {
  p2.x.long <- tl[[i]]$x
  p2.y.lat <- tl[[i]]$y
  p2.length <- length(p2.x.long)
  p2 <- data.frame(PID=rep(2, p2.length), POS=1:p2.length, X= p2.x.long , Y= p2.y.lat ) 
  polygon(p2$X, p2$Y, border="black", lty="solid", lwd=.5, col=cols[i]) #cols[i]
}

