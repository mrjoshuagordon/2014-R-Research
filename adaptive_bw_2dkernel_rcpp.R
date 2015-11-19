# require(Rcpp) 
cppFunction('
NumericVector myKern2Dcpp(NumericVector x, NumericVector y, int nbins, NumericVector h, NumericVector lims) {
  double delta_x = 0;
  double delta_y = 0;
  delta_x = (lims[1] - lims[0])/(nbins - 1);
  delta_y = (lims[3] - lims[2])/(nbins - 1);
  int N = x.size();
  double NN = x.size();
  
  NumericVector grid_x(nbins);
  NumericVector grid_y(nbins); 

  int nbins_mu = nbins * nbins;
  NumericVector mu(nbins_mu);

  for(int i =0; i <= nbins; i++) {
    grid_x[i] = lims[0] + delta_x * i;
  }

  for(int i =0; i <= nbins; i++) {
    grid_y[i] = lims[2] + delta_y * i;
  }

  int ind = 0;
  double sum = 0, xmid = 0, ymid = 0;
  for(int i=0; i<nbins; i++) {
      xmid = grid_x[i];
    for(int j=0; j<nbins; j++) {
      ymid = grid_y[j];
      sum = 0;
      for(int k=0; k<N; k++) {
      double xx = xmid - x[k];
      double yy = ymid - y[k];
        sum += ((1.0 / (2.0*PI*h[k]*h[k])) * exp(-1.0 * (xx*xx + yy*yy) / (2.0*h[k]*h[k])))/NN;
      }
      mu[ind] = sum;
      ind++;
    }
  }
  return mu;
}     	
            ')

# R wrapper 
my2dKern2 <- function(x, y, nbins, h, lims){
  result <- myKern2Dcpp(x = x, y = y, nbins = nbins,  h = h, lims = lims)
  result <- matrix(result, nrow = nbins, ncol = nbins, byrow = TRUE)
  return(result)
}


x <- c(0, 1, 1.1, 1.5, 1.9, 2.8, 2.9, 3.5)
y <- c(0, 1, 1.1, 1.5, 1.9, 2.8, 2.9, 3.5)
h <- rep(.5, length(x))
nx <- 10
lims <- c(range(x), range(y)) 
my2dKern2(x = x , y = y, h = h, nbins = nx, lims = lims )

# require(rbenchmark); require(MASS)
benchmark(my2dKern2(x = x, y = y, nbins = 10,  h = rep(.5, length(x)), lims = c(range(x), range(y))),
          kde2d(x = x, y = y, h = c(2,2), n = 10,  lims = c(range(x), range(y))))

