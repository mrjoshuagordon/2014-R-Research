pp1=rpoispp(lambda = 100,win=owin(c(-1,1),c(-1,1)));
plot(pp1)





pp2=rpoispp(lambda= 100 ,win=owin(c(-1,1),c(-1,1)));
p2x = pp2$x[-(which(sqrt(abs(pp2$x)^2 + abs(pp2$y)^2)<.35))]
p2y = pp2$y[-(which(sqrt(abs(pp2$x)^2 + abs(pp2$y)^2)<.35))]

plot(p2x, p2y)




plot(deldir(pp1$x, pp1$y), wlines=c('tess'), fill=colorRampPalette(brewer.pal(9,"Greens"))(length(pp1$x)) ) 

plot( tile.list(pp2.d), polycol = heat.colors(6), close=TRUE )




par(mfrow=c(1,1))


# integral of contstant intensity at 100 
pp2.d = deldir(p2x,p2y)
plot(pp2.d, wlines=c('tess'), wpoints='none', xlim=c(-1,1), ylim=c(-1,1)) 
#plot(pp2.d, wlines=c('tess'), fill=colorRampPalette(brewer.pal(9,"Blues"))(length(p2x))) 
text( p2x, p2y, labels = 1:length(p2x), adj = c(0,0), cex=.5 ) 

sol = data.frame(1:length(pp2.d$summary$del.area), 100* pp2.d$summary$del.area)
names(sol) =c("index", "area")



sol[202,]
length(which(sol[,2] <1))/length(sol[,2])


lambda.fun2 = function(x,y){
  
100 
} 
pp3=rpoispp(lambda = lambda.fun2,win=owin(c(-1,1),c(-1,1)));
plot(pp3) 



####  integral 

sapply(y, function(y) {
  +     integrate(function(x) {200 * x^2  * abs(y)},lower = , upper = y)$value
  +   })
+ }, 0, .5)
