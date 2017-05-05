library(globalOptTests)
library(GA)
library(TSP)

FITNESS_BECKER_LAGO = function(x) { goTest(fnName="BeckerLago", par=x) }
MIN_BECKER_LAGO = c(-5, -5)
MAX_BECKER_LAGO = c(5, 5)
BEST_SOLUTION_BECKER_LAGO = 50

#zbiór parametrów do badań
MUTATIONS <- c(0, 0.2, 0.4, 0.6, 0.8, 1)
CROSSOVERS <- c(0, 0.2, 0.4, 0.6, 0.8, 1)
POPULATIONS <- c(10, 20, 50, 100, 200, 500)

plotNumber = 1

setwd("/Users/mikolaj/projekty/r/inteligencja_obliczeniowa")

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

singlePointCrossover <- function(object, parents)
{
  fitness <- object@fitness[parents]
  parents <- object@population[parents,,drop = FALSE]
  
  n <- ncol(parents)
  children <- matrix(as.double(NA), nrow = 2, ncol = n)
  fitnessChildren <- rep(NA, 2)
  
  
  crossOverPoint1 <- sample(1:(n-1), size = 1)
  
  children[1,] <- c(parents[1,1:crossOverPoint1], parents[2,(crossOverPoint1+1):n])
  
  children[2,] <- c(parents[2,1:crossOverPoint1], parents[1,(crossOverPoint1+1):n])
  
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
runGA <- function(type = "real-valued", ...) {
  GA <- ga(monitor = FALSE, type = type, ...)
  
  return(GA@fitnessValue)
}

#wykonanie algorytmu genetycznego pięciokrotnie i zwrócenie uśrednionej wartości dopasowania
getAverageResult <- function(iterations_count, ...) {
  results <- rep(0, iterations_count)
  for(i in 1:iterations_count) {
    results[i] <- runGA(...)
  }
  return(mean(results))
}

drawPlot <- function(max, x, results, xlab, main, legendLabels) {
  plotNumber = plotNumber + 1
  colors<-c(rgb(0,0,1,0.7), rgb(0,1,0,0.7), rgb(0,1,1,0.7), rgb(1,1,0,0.7), rgb(1,0,0,1))
  colCount = length(colors)
  resCount = length(results[,1])
  plot(plotNumber, xlim=range(x), ylim=range(c(results, max)), type="l", xlab=xlab, ylab='Fitness value', main=main)
  
  lapply(seq_along(results[1:(resCount)]),function(i){
    lines(x, results[i,], col=colors[i])
    points(x, results[i,])
  })
  lines(x, rep(max, length(x)), col=colors[resCount+1])
  legend("bottomleft", legend = legendLabels, col=colors, pch=1)
}



runCustomMethodsTests <- function() {

  iterations_count = 30
  results = matrix(nrow = 2, ncol = 6)
  
  csv <- paste("Mutation probability", "Default mutation method", "Custom mutation method", sep = ";")
  #przeprowadzenie badań dla zmiennej wartości prawdopodobieństwa mutacji
  for(i in 1:length(MUTATIONS)) {
    results[1,i] = getAverageResult(iterations_count, fitness = FITNESS_BECKER_LAGO, min = MIN_BECKER_LAGO,
                                    max = MAX_BECKER_LAGO, pmutation = MUTATIONS[i])
    results[2,i] = getAverageResult(iterations_count, fitness = FITNESS_BECKER_LAGO, min = MIN_BECKER_LAGO,
                                    max = MAX_BECKER_LAGO, pmutation = MUTATIONS[i], mutation = randomMutation)
    cat(results)
    row <- paste(MUTATIONS[i], results[1,i], results[2,i], sep = ";")
    csv <- paste(csv, row, sep = "\n")
    #zapisanie otrzymanego wyniku wraz z parametrami
  }
  
  write.csv(csv, file = paste("cmutation", ".csv", sep = ""))
  drawPlot(BEST_SOLUTION_BECKER_LAGO, MUTATIONS, results, "Mutation probability", "Mutation", c("Default", "Custom", "Max"))
  
  csv <- paste("Crossover probability", "Default crossover method", "Custom crossover method", sep = ";")
  #przeprowadzenie badań dla zmiennej wartości prawdopodobieństwa krzyżowania
  for(i in 1:length(CROSSOVERS)) {
    results[1,i] = getAverageResult(iterations_count, fitness = FITNESS_BECKER_LAGO, min = MIN_BECKER_LAGO,
                                    max = MAX_BECKER_LAGO, pcrossover = CROSSOVERS[i])
    results[2,i] = getAverageResult(iterations_count, fitness = FITNESS_BECKER_LAGO, min = MIN_BECKER_LAGO,
                                    max = MAX_BECKER_LAGO, pcrossover = CROSSOVERS[i],
                                    crossover = doublePointCrossover)
    cat(results)
    row <- paste(MUTATIONS[i], results[1,i], results[2,i], sep = ";")
    csv <- paste(csv, row, sep = "\n")
  }
  
  write.csv(csv, file = paste("ccrossover", ".csv", sep = ""))
  drawPlot(BEST_SOLUTION_BECKER_LAGO, CROSSOVERS, results, "Crossover probability", "Crossover", c("Default", "Custom", "Max"))
  
  csv <- paste("Population size", "Default selection method", "Custom selection method", sep = ";")
  #przeprowadzenie badań dla zmiennej wielkości populacji
  print("***POPSIZE***")
  for(i in 1:length(POPULATIONS)) {
    results[1,i] = getAverageResult(iterations_count, fitness = FITNESS_BECKER_LAGO, min = MIN_BECKER_LAGO,
                                    max = MAX_BECKER_LAGO, popSize = POPULATIONS[i])
    results[2,i] = getAverageResult(iterations_count, fitness = FITNESS_BECKER_LAGO, min = MIN_BECKER_LAGO,
                                    max = MAX_BECKER_LAGO, popSize = POPULATIONS[i], selection = tournamentSelection)
    cat(results)
    row <- paste(MUTATIONS[i], results[1,i], results[2,i], sep = ";")
    csv <- paste(csv, row, sep = "\n")
  }
  
  write.csv(csv, file = paste("cselection", ".csv", sep = ""))
  drawPlot(BEST_SOLUTION_BECKER_LAGO, POPULATIONS, results, "Population size", "Selection", c("Default", "Custom", "Max"))
}

runTSPTests <- function(tsp, filename, tsp_range, best_solution) {
  iterations_count = 15
  results = matrix(nrow = 1, ncol = 6)
  
  fitness <- function(x) {
    tour <- TOUR(x, tsp=tsp)
    return(tour_length(tour, tsp=tsp))
  }
  
  fitnessWrapper <- function(x) {
    return(-fitness(x))
  }
  
  min = tsp_range[1]
  max = tsp_range[2]
  
  csv <- paste("Mutation probability", "Result", sep = ";")
  #przeprowadzenie badań dla zmiennej wartości prawdopodobieństwa mutacji
  for(i in 1:length(MUTATIONS)) {
    results[1,i] = -getAverageResult(iterations_count, type = "permutation", fitness = fitnessWrapper, min = min, max = max, 
                                    pmutation = MUTATIONS[i])
    row <- paste(MUTATIONS[i], results[1,i], sep = ";")
    print(results)
    #zapisanie otrzymanego wyniku wraz z parametrami
    csv <- paste(csv, row, sep = "\n")
  }
  
  write.csv(csv, file = paste("mutation_", filename, ".csv", sep = ""))
  drawPlot(best_solution, MUTATIONS, results, "Mutation probability",
           paste(filename, "mutation", sep=" "), c("Result", "Min"))
  
  csv <- paste("Crossover probability", "Result", sep = ";")
  #przeprowadzenie badań dla zmiennej wartości prawdopodobieństwa krzyżowania
  for(i in 1:length(CROSSOVERS)) {
    results[1,i] = -getAverageResult(iterations_count, type = "permutation", fitness = fitnessWrapper, min = min, max = max, 
                                     pcrossover = CROSSOVERS[i])
    row <- paste(CROSSOVERS[i], results[1,i], sep = ";")
    print(results)
    #zapisanie otrzymanego wyniku wraz z parametrami
    csv <- paste(csv, row, sep = "\n")
  }
  
  write.csv(csv, file = paste("crossover_", filename, ".csv", sep = ""))
  drawPlot(best_solution, CROSSOVERS, results, "Crossover probability",
           paste(filename, "crossover", sep=" "), c("Result", "Min"))
  
  
  csv <- paste("Population size", "Result", sep = ";")
  #przeprowadzenie badań dla zmiennej wielkości populacji
  for(i in 1:length(POPULATIONS)) {
    results[1,i] = -getAverageResult(iterations_count, type = "permutation", fitness = fitnessWrapper, min = min, max = max, 
                                     popSize = POPULATIONS[i])
    row <- paste(POPULATIONS[i], results[1,i], sep = ";")
    print(results)
    #zapisanie otrzymanego wyniku wraz z parametrami
    csv <- paste(csv, row, sep = "\n")
  }
  
  write.csv(csv, file = paste("popsize_", filename, ".csv", sep = ""))
  drawPlot(best_solution, POPULATIONS, results, "Population size",
           paste(filename, "population size", sep=" "), c("Result", "Min"))
  
}
  
#wykonanie badań dla zadanej funkcji w podanym przedziale
runOptimTests <- function() {
  iterations_count = 30
  #zbiór parametrów do badań
  poptims <- c(0.00, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 0.45, 0.5, 0.55, 0.6, 0.65, 0.7, 0.75,
               0.8, 0.85, 0.9, 0.95, 1)

  csv <- paste("Local search probability", "Default methods", "Custom crossover method",
               "Custom mutation method", "Custom selection method", sep = ";")
  
  results = matrix(nrow = 4, ncol = length(poptims))
  
  for(i in 1:length(poptims)) {
    
    results[1,i] = getAverageResult(1, optimArgs = list(poptim = poptims[i]), optim=TRUE,
                                    fitness = FITNESS_BECKER_LAGO, min = MIN_BECKER_LAGO,
                                    max = MAX_BECKER_LAGO)
    results[2,i] = getAverageResult(1, optimArgs = list(poptim = poptims[i]), optim=TRUE,
                                    crossover = doublePointCrossover, fitness = FITNESS_BECKER_LAGO,
                                    min = MIN_BECKER_LAGO,max = MAX_BECKER_LAGO)
    results[3,i] = getAverageResult(1, optimArgs = list(poptim = poptims[i]), optim=TRUE, mutation = randomMutation,
                                    fitness = FITNESS_BECKER_LAGO, min = MIN_BECKER_LAGO,
                                    max = MAX_BECKER_LAGO)
    results[4,i] = getAverageResult(1, optimArgs = list(poptim = poptims[i]), optim=TRUE,
                                    selection = tournamentSelection, fitness = FITNESS_BECKER_LAGO,
                                    min = MIN_BECKER_LAGO, max = MAX_BECKER_LAGO)
    print(results)
    row <- paste(poptims[i], results[1,i], results[2,i], results[3,i], results[4,i], sep = ";")
    csv <- paste(csv, row, sep = "\n")
    
  }
  
  write.csv(csv, file = paste("optim", ".csv", sep = ""))
  drawPlot(BEST_SOLUTION_BECKER_LAGO, poptims, results, "Local search probability", "Local search optimization with custom methods",
           c("Default", "Custom crossover", "Custom mutation", "Custom selection", "Max"))
}
  
  
  
#wykonanie badań dla wybranych funkcji
#runCustomMethodsTests()

#tsp <- read_TSPLIB("bayg29.tsp")
#tsp_range <- c(1, 29)
#runTSPTests(tsp, "bayg29", tsp_range, 1610)

#tsp <- read_TSPLIB("berlin52.tsp")
#tsp_range <- c(1, 52)
#runTSPTests(tsp, "berlin52", tsp_range, 7542)

tsp <- read_TSPLIB("hk48.tsp")
tsp_range <- c(1, 48)
runTSPTests(tsp, "hk48", tsp_range, 11461)


#runOptimTests()
  