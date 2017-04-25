library(globalOptTests)
library(GA)

ITERATIONS_COUNT <- 30

tournamentSelection <- function(object) {
  tournamentPopSize = object@popSize/20
  if(tournamentPopSize <= 1) {
    tournamentPopSize = 3
  }
  
  selected <- rep(NA, object@popSize)
  
  for(i in 1:object@popSize) {
    tournament <- sample(1:object@popSize, tournamentPopSize)
    selected[i] <- tournament[which.max(object@fitness[tournament])]
  }
  
  out <- list(population = object@population[selected,,drop=FALSE],
              fitness = object@fitness[selected])
  return(out)
}

doublePointCrossover <- function(object, parents)
{
  fitness <- object@fitness[parents]
  parents <- object@population[parents,,drop = FALSE]
  
  n <- ncol(parents)
  children <- matrix(as.double(NA), nrow = 2, ncol = n)
  fitnessChildren <- rep(NA, 2)
  
  
  crossOverPoint1 <- sample(1:(n-1), size = 1)
  
  if(length(parents[1,]) <= 2) {
    children[1,] <- c(parents[1,1:crossOverPoint1], parents[2,(crossOverPoint1+1):n])
    
    children[2,] <- c(parents[2,1:crossOverPoint1], parents[1,(crossOverPoint1+1):n])
  } else {
    repeat{
      crossOverPoint2 <- sample(0:n-1, size = 1)
      if(crossOverPoint1 != crossOverPoint2){
        break
      }
    }
    
    if(crossOverPoint1 > crossOverPoint2) { 
      x = crossOverPoint1
      crossOverPoint1 = crossOverPoint2
      crossOverPoint2 = x
    }
    children[1,] <- c(parents[1,1:crossOverPoint1],
                        parents[2,(crossOverPoint1+1):crossOverPoint2],
                        parents[1,(crossOverPoint2+1):n])
    
    children[2,] <- c(parents[2,1:crossOverPoint1],
                      parents[1,(crossOverPoint1+1):crossOverPoint2],
                      parents[2,(crossOverPoint2+1):n])
  }

  out <- list(children = children, fitness = fitnessChildren)
  return(out)
}

randomMutation <- function(object, parent)
{
  mutate <- parent <- as.vector(object@population[parent,])
  n <- length(parent)
  j <- sample(1:n, size = 1)
  mutate[j] <- runif(1, object@min[j], object@max[j])
  return(mutate)
}

#wywołanie funkcji GA z podanymi parametrami dla funkcji z pakietu globalOptTest i zwrócene wartości dopasowania
runGA <- function(popSize, pmutation, pcrossover, ...) {
  GA <- ga(type = "real-valued",
           fitness = function(x) { goTest(fnName="BeckerLago", par=x) },
           min = c(-5, -5), max = c(5, 5), monitor = FALSE, 
           popSize = popSize, pmutation = pmutation,
           pcrossover = pcrossover, ...)
  
  return(GA@fitnessValue)
}

#wykonanie algorytmu genetycznego pięciokrotnie i zwrócenie uśrednionej wartości dopasowania
getAverageResult <- function(popSize, pmutation, pcrossover, ...) {
  results <- rep(0, ITERATIONS_COUNT)
  for(i in 1:ITERATIONS_COUNT) {
    results[i] <- runGA(popSize, pmutation, pcrossover, ...)
  }
  return(mean(results))
}

#wykonanie algorytmu genetycznego z podanymi parametrami i zwrócenie uśrednienionych wyników
logResults <- function(popSize, pmutation, pcrossover, ...) {
  fitnessValue <- getAverageResult(popSize, pmutation, pcrossover, ...)
  print(paste("popSize: ", popSize, ", pmutation: ", pmutation,
              ", pcrossover: ", pcrossover, ", fitnessValue: ", fitnessValue))
  return(fitnessValue)
}

drawPlot <- function(plotNumber, x, results, xlab, main) {
  colCount = 3
  Col.rar1<-c(rgb(0,0,1,0.7), rgb(0,1,0,0.7), rgb(1,0,0,1))
  plot(plotNumber, xlim=range(x), ylim=range(results), type="l", xlab=xlab, ylab='Fitness value', main=main)
  
  lapply(seq_along(Col.rar1[1:(colCount-1)]),function(i){
    lines(x, results[i,], col=Col.rar1[i])
    points(x, results[i,])
  })
  lines(x, rep(50, 6), col=Col.rar1[colCount])
  legend("bottomleft", legend = c("Default", "Custom", "Max value"), col=Col.rar1, pch=1)
}
  
  #wykonanie badań dla zadanej funkcji w podanym przedziale
  runTests <- function() {
    #zbiór parametrów do badań
    mutations <- c(0, 0.2, 0.4, 0.6, 0.8, 1)
    crossovers <- c(0, 0.2, 0.4, 0.6, 0.8, 1)
    populations <- c(10, 20, 50, 100, 200, 500)
    
    #domyślne parametry dla algorytmu Genetycznego
    popSizeDefault <- 50
    pmutationDefault <- 0.1
    pcrossoverDefault <- 0.8
    
    results = matrix(nrow = 2, ncol = 6)
    
    #przeprowadzenie badań dla zmiennej wartości prawdopodobieństwa mutacji
    print("***MUTATION***")
    for(i in 1:length(mutations)) {
      results[1,i] = logResults(popSizeDefault, mutations[i], pcrossoverDefault)
      results[2,i] = logResults(popSizeDefault, mutations[i], pcrossoverDefault, mutation = randomMutation)
      cat("\n")
      #zapisanie otrzymanego wyniku wraz z parametrami
    }
    
    drawPlot(1, mutations, results, "Mutation probability", "Mutation")

    
    #przeprowadzenie badań dla zmiennej wartości prawdopodobieństwa krzyżowania
    print("***CROSSOVER***")
    for(i in 1:length(crossovers)) {
      results[1,i] = logResults(popSizeDefault, pmutationDefault, crossovers[i])
      results[2,i] = logResults(popSizeDefault, pmutationDefault, crossovers[i], crossover = doublePointCrossover)
      cat("\n")
    }
    
    drawPlot(2, mutations, results, "Crossover probability", "Crossover")
    
    #przeprowadzenie badań dla zmiennej wielkości populacji
    print("***POPSIZE***")
    for(i in 1:length(populations)) {
      results[1,i] = logResults(populations[i], pmutationDefault, pcrossoverDefault)
      results[2,i] = logResults(populations[i], pmutationDefault, pcrossoverDefault, selection = tournamentSelection)
      cat("\n")
    }
    
    drawPlot(3, mutations, results, "Population size", "Selection")
  }
  
  #wykonanie badań dla wybranych funkcji
  runTests()
  