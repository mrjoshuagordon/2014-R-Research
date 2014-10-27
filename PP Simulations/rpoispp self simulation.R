
lambda.fun1 = function(x,y){200*(x^2)*abs(y)}
lambda.fun = function(x,y){(x^2)*abs(y)}

pp1=rpoispp(lambda = lambda.fun1,win=owin(c(-1,1),c(-1,1)));
plot(pp1)





##############  SLOW #########

lambda <- 1
upper <- 200
#N <- rpois(1, lambda * upper)
Tn <- rexp(N, lambda)

N=1000
Un <- runif(N)
x = seq(-2,2,length=N)
y = seq(-2,2,length=N) 

xy = x %*% t(y)

# xs = matrix(nrow=length(x), ncol=length(y), 0)
# for(i in 1:length(x)){
#   for(j in 1:length(y)){
#     xs[,i][j] = lambda.fun(x[i], y[j])
#   }
#   if(i%%10 == 0) (print(i))
# }
# 


keep <- (Un <= x^2 * abs(y))
plot(x[keep], y[keep])

nx = x[keep]
ny = y[keep]

plot(0,0, ylim=c(-2,2), xlim=c(-2,2), type="n")
v= matrix(nrow=length(nx)*length(nx), ncol=2)
count = 1
for(i in 1:length(nx)){
  for(j in 1:length(ny)){
    v[count,] =  c(nx[i], ny[j])
    count = count + 1
  }
  if(i%%10 == 0) (print(i))
}


nv = v[sample(1:nrow(v),size=length(pp1$x)),]

plot(nv[,1], nv[,2])


dev.off()
par(mfrow=c(1,1))
hist(pp1$x)
hist(nv[,1])