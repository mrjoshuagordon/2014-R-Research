lambda <- function(x, y) { 100 * (x + y) }
b = rpoispp(lambda)
plot(b$x, b$y)



x = seq(0,1,length=100)
y = seq(0,1,length=100)


xs = matrix(nrow=length(x), ncol=length(y), 0)
for(i in 1:length(x)){
   for(j in 1:length(y)){
      xs[,i][j,] = lambda(x[i], y[j])
    }
}



