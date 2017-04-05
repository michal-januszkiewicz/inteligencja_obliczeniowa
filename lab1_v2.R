library(globalOptTests)

drawFunction <- function(functionName, x1, x2, y1, y2) {
  
  x <- seq(x1, x2, 0.1)
  y <- seq(y1, y2, 0.1)
  
  z <- matrix(nrow=length(x), ncol=length(y))
  for(i in 1:length(x)) {
    for(j in 1:length(y)) {
      z[i,j] <- goTest(fnName=functionName, par=c(x[i], y[j]))
    }
  }
  
  persp3D(x,y,z)
}

runGA <- function(popSize, maxiter, pmutation, pcrossover, elitism, functionName, x1, x2, y1, y2) {
  GA <- ga(type = "real-valued",
           fitness = function(x) { goTest(fnName=functionName, par=x) },
           min = c(x1, y1), max = c(x2, y2), monitor = FALSE, 
           popSize = popSize, maxiter = maxiter, pmutation = pmutation,
           pcrossover = pcrossover, elitism = elitism)
  
  return(GA@fitnessValue)
}

getAverageResult <- function(popSize, maxiter, pmutation, pcrossover, elitism, functionName, x1, x2, y1, y2) {
  results <- c(0, 0, 0, 0, 0)
  for(i in 1:5) {
    results[i] <- runGA(popSize, maxiter, pmutation, pcrossover, elitism, functionName, x1, x2, y1, y2)
  }
  return(mean(results))
}

logResults <- function(popSize, maxiter, pmutation, pcrossover, elitism, functionName, x1, x2, y1, y2) {
  fitnessValue <- getAverageResult(popSize, maxiter, pmutation, pcrossover, elitism, functionName, x1, x2, y1, y2)
  print(paste("popSize: ", popSize, ", maxiter: ", maxiter, ", pmutation: ", pmutation,
              ", pcrossover: ", pcrossover, ", elitism: ", elitism, ", fitnessValue: ", fitnessValue))
  return(paste(popSize, maxiter, pmutation, pcrossover, elitism, fitnessValue, sep = " "))
}

runTestsForFunction <- function(functionName, x1, x2, y1, y2) {
  drawFunction(functionName, x1, x2, y1, y2)
  #zbiór parametrów do badań
  mutations <- c(0, 0.2, 0.4, 0.6, 0.8, 1)
  crossovers <- c(0, 0.2, 0.4, 0.6, 0.8, 1)
  elitisms <- c(0, 2, 5, 10, 20, 40)
  iterations <- c(10, 20, 50, 100, 1000)
  populations <- c(10, 20, 50, 100, 1000)
  
  #domyślne parametry
  popSizeDefault <- 50
  maxiterDefault <- 100
  pmutationDefault <- 0.1
  pcrossoverDefault <- 0.8
  elitismDefault  <- 2 # to się zmienia w zależności od rozmiaru populacji
  
  csv <- paste("popSize", "maxiter", "pmutation", "pcrossover", "elitism", "fitnessValue", sep = " ")
  
  print("***MUTATION***")
  for(pmutation in mutations) {
    row = logResults(popSizeDefault, maxiterDefault, pmutation, pcrossoverDefault,
               elitismDefault, functionName, x1, x2, y1, y2)
    cat("\n")
    csv <- paste(csv, row, sep = "\n")
  }
  
  print("***CROSSOVER***")
  for(pcrossover in crossovers) {
    row = logResults(popSizeDefault, maxiterDefault, pmutationDefault, pcrossover,
               elitismDefault, functionName, x1, x2, y1, y2)
    cat("\n")
    csv <- paste(csv, row, sep = "\n")
  }
  
  print("***ELITISM***")
  for(elitism in elitisms) {
    row = logResults(popSizeDefault, maxiterDefault, pmutationDefault, pcrossoverDefault,
               elitism, functionName, x1, x2, y1, y2)
    cat("\n")
    csv <- paste(csv, row, sep = "\n")
  }
  
  print("***MAXITER***")
  for(maxiter in iterations) {
    row = logResults(popSizeDefault, maxiter, pmutationDefault, pcrossoverDefault,
               elitismDefault, functionName, x1, x2, y1, y2)
    cat("\n")
    csv <- paste(csv, row, sep = "\n")
  }
  
  print("***POPSIZE***")
  for(popSize in populations) {
    row = logResults(popSize, maxiterDefault, pmutationDefault, pcrossoverDefault,
               elitismDefault, functionName, x1, x2, y1, y2)
    cat("\n")
    csv <- paste(csv, row, sep = "\n")
  }
  
  write.csv(csv, file = paste(functionName, ".csv", sep = ""))
}

setwd("/home/michal")

#runTestsForFunction("BeckerLago", -5, 5, -5, 5)
runTestsForFunction("AluffiPentini", -2, 2, -2, 2)
