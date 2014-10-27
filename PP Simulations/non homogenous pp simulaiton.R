## Libraries 

library(spatstat)
library(deldir)
library(datautils)
library(RColorBrewer) 

#Simulate Non Homogenous PP using spatstat

lambda.fun1 = function(x,y){200*(x^2)*abs(y)} 
pp1=rpoispp(lambda = lambda.fun1,win=owin(c(-1,1),c(-1,1)));
plot(pp1)


# Simulate Non Homogenous PP using own method 
N=length(pp1$x)
x = seq(-1,1,length=N)
y = seq(-1,1,length=N)  

xs = (200* x^2 %*% t(abs(y)) )  
xs1 = xs / max(xs)
dim(xs)

# generate uniform random variables for rejection criterion 
Un = matrix(nrow=dim(xs1)[1], ncol=dim(xs1)[2], runif(dim(xs1)[1]*dim(xs1)[2]))
h  = Un <= xs1

# store points where pass rejection 
# initiale matrix for storing points which pass the rejection region 
v= matrix(nrow=length(which(h==T)), ncol=2) 
count = 0 
for(i in 1:ncol(h)){
  for(j in 1:nrow(h)){
   if(h[,i][j] == TRUE) {
     v[count,] = c(x[i], y[j])
     count = count +1 
     if(count%%1000==0) print(count)
   }
   
    
  }  
}
v = na.omit(v)
#head(v)
#tail(v)
nv = v[sample(1:nrow(v),size=length(pp1$x)),]
#nv = v[sample(1:nrow(v),size=2000),]


dev.off()
par(mfrow=c(1,2))
plot(nv[,1], nv[,2], xaxt="n", yaxt="n", ylim=c(-1,1), xlim=c(-1,1))
plot(pp1)


dev.off()
par(mfrow=c(1,2))
#spatstat
plot(deldir(pp1$x, pp1$y), wlines=c('tess'), fill=colorRampPalette(brewer.pal(9,"Blues"))(length(pp1$x)) )

#josh's
plot(deldir(nv[,1], nv[,2]), wlines=c('tess'), fill=colorRampPalette(brewer.pal(9,"Greens"))(length(pp1$x)) )  








#hist(pp1$x)
#hist(nv[,1])


length(which(as.vector(h)== TRUE))/length(as.vector(h))
