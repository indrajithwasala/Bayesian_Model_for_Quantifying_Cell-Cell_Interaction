# uniform Poisson process with intensity 100 in the unit square
pp <- rpoispp(100)

# uniform Poisson process with intensity 1 in a 10 x 10 square
pp <- rpoispp(1, win=owin(c(0,10),c(0,10)))
# plots should look similar !

# inhomogeneous Poisson process in unit square
# with intensity lambda(x,y) = 100 * exp(-3*x)
# Intensity is bounded by 100
pp <- rpoispp(function(x,y) {100 * exp(-3*x)}, 100)

# How to tune the coefficient of x
lamb <- function(x,y,a) { 100 * exp( - a * x)}
pp <- rpoispp(lamb, 100, a=3)

# pixel image
Z <- as.im(function(x,y){100 * sqrt(x+y)}, unit.square())
pp <- rpoispp(Z)

# randomising an existing point pattern
rpoispp(intensity(cells), win=Window(cells))
rpoispp(ex=cells)


plot(rpoispp(100, win = letterR))

#######################################################################################################

Z1 <- rpoispp(10,win=owin(poly = list(list(x = c(0, 8, 8, 0), y = c(0, 0, 8, 8)), list(x = c(2, 2, 6, 6), y = c(2, 6, 6, 2)))))
Z2<-rpoispp(50,win=owin(c(2,6),c(2,6)))

plot(Z1,pch=19)
plot(Z2,pch=23,add=TRUE)
coords(Z1)
coords(Z2)



z_generator = function(x, y, Q, Theta, lambda, c, omega, iter, seed) {
  set.seed(seed);
  
  # Generate imaging data
  build <- dist_list(x, y, c);
  edge <- build$edge;
  distance <- build$distance;
  duplicate <- build$duplicate;
  flag_start <- build$flag_start;
  flag_end <- build$flag_end;
  
  # Generate cell types
  n <- length(x);
  z_s <- sample(1:Q, n, replace = TRUE, prob = exp(-omega)/sum(exp(-omega)));
  z <- z_s;
  # energy_s <- logenergy(z_s, edge, distance, duplicate, Thetat, lambdat);
  # energy <- rep(NA, iter);
  count <- 10;
  for (i in 1:iter) {
    if (i/iter*100 == count) {
      print(paste0("Simulating the data ------ ", count, "% has been done"));
      count <- count + 10;
    }
    z <- model(z, edge, distance, 1, n, flag_start, flag_end, Theta, omega, lambda);
    # energy[i] <- logenergy(z, edge, distance, duplicate, Thetat, lambdat);
  }
  return (z); 
}

require(spatstat);
require(Rcpp);

# Load functions
source('functions.R');
Rcpp::sourceCpp('functions_x2.cpp');

Th1<-matrix(c(1,1.2,1.5,1.2,1.7,1.6,1.5,1.6,1),nrow = 3,byrow = T)
Om1<-c(1,1.5,1.7)

x<-coords(Z1)[1]
x<-unlist(x, use.names=FALSE)
y<-coords(Z1)[2]
y<-unlist(y, use.names=FALSE)

z<-z_generator(x,y,3,Th1,60,0.5,Om1,100,2)[,1]
data1<-data.frame(x,y,z)


Th2<-matrix(c(1.5,1.8,1.5,1.8,1.7,1.3,1.5,1.3,2),nrow = 3,byrow = T)
Om2<-c(1,1.8,2.7)

x<-coords(Z2)[1]
x<-unlist(x, use.names=FALSE)
y<-coords(Z2)[2]
y<-unlist(y, use.names=FALSE)

z<-z_generator(x,y,3,Th2,60,0.5,Om2,100,2)[,1]
data2<-data.frame(x,y,z)
dataAll<-rbind(data1,data2)
#x<-c(0.2,3.4,6.3,2,4.5)
#y<-c(2.5,6,3.7,4.2,2)


par(pty = "s", mar=c(4.5, 2.5, 1, 1));
plot(dataAll$x, dataAll$y,  col = dataAll$z, pch = data1$z, 
     main = "", xlab = "x", ylab = "y", cex.lab = 1.5, cex.axis = 1.5, cex = 0.5)

