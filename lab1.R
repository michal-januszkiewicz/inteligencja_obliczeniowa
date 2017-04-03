library(GA)
f <- function(x)  (x^2 - 10 * cos(2 * pi * x) + 10)
min <- -25; max <- 25

mutations <- c(0, 0.2, 0.4, 0.6, 0.8, 1)
crossovers <- c(0, 0.2, 0.4, 0.6, 0.8, 1)
elitisms <- c(0, 2, 5, 10, 20, 40)
iterations <- c(10, 20, 50, 100, 1000)
populations <- c(10, 20, 50, 100, 1000)

popSize <- 50
maxiter <- 100
pmutation <- 0.1
pcrossover <- 0.8
elitism  <- 2

setwd("/home/michal")

csv <- paste("popSize", "maxiter", "pmutation", "pcrossover", "elitism", "fitnessValue", sep = " ")
for(pmutation in mutations) {
  y <- 0
  for(i in 1:5) {
    GA <- ga(type = "real-valued", fitness = f, min = min, max = max, monitor = FALSE, 
             popSize = popSize, maxiter = maxiter, pmutation = pmutation, pcrossover = pcrossover, elitism = elitism)
    
    y <- y + GA@fitnessValue
  }
  y <- y/5
  print(y)
  row <- paste(popSize, maxiter, pmutation, pcrossover, elitism, y, sep = " ")
  csv <- paste(csv, row, sep = "\n")
}
write.csv(csv, file = "mutations.csv")

GA <- ga(type = "real-valued", fitness = f, min = min, max = max, monitor = FALSE, 
         popSize = 200, maxiter = 200, pmutation = 0.2, pcrossover = 0.8)
y <- GA@fitnessValue
print(y)

curve(f, min, max, n = 1000)
points(GA@solution, GA@fitnessValue, col = 2, pch = 19)
