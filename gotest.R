library(globalOptTests)
library(GA)

f <- function(a) {
  goTest(fnName="AluffiPentini", par=a)
}

min1 <- -10; min2 <- -5
max1 <- 10; max2 <- 20
min <- c(min1, min2); max <- c(max1, max2)

popSize <- 50
maxiter <- 100
pmutation <- 0.1
pcrossover <- 0.8
elitism  <- 2

GA <- ga(type = "real-valued", fitness = f, min = min, max = max, monitor = FALSE, 
         popSize = popSize, maxiter = maxiter, pmutation = pmutation, pcrossover = pcrossover, elitism = elitism)

x <- seq(min1, min2, 0.1)
y <- seq(max1, max2, 0.1)

z <- matrix(nrow=length(x), ncol=length(y))
for(i in 1:length(x)) {
  for(j in 1:length(y)) {
    z[i,j] <- f(c(x[i], y[j]))
  }
}

persp3D(x,y,z)

#filled.contour(x,y,z)