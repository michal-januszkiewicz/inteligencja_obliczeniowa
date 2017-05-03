library(globalOptTests)
library(GA)
library(TSP)

ITERATIONS_COUNT <- 15
setwd("/home/michal/R/x86_64-pc-linux-gnu-library/lab/tsp")

#wywołanie funkcji GA z podanymi parametrami dla funkcji z pakietu globalOptTest i zwrócene wartości dopasowania
runGA <- function(popSize, pmutation, pcrossover, tsp, tsp_range) {
  f <- function(x) {
    tour <- TOUR(x, tsp=tsp)
    return(tour_length(tour, tsp=tsp))
  }
  
  g <- function(x) {
    return(-f(x))
  }
  
  GA <- ga(type = "permutation",
           fitness = g,
           min = tsp_range[1], max = tsp_range[2],
           popSize = popSize, pmutation = pmutation,
           pcrossover = pcrossover)
  
  return(GA@fitnessValue)
}

#wykonanie algorytmu genetycznego pięciokrotnie i zwrócenie uśrednionej wartości dopasowania
getAverageResult <- function(popSize, pmutation, pcrossover, tsp, tsp_range) {
  results <- rep(0, ITERATIONS_COUNT)
  for(i in 1:ITERATIONS_COUNT) {
    results[i] <- runGA(popSize, pmutation, pcrossover, tsp, tsp_range)
  }
  return(mean(results))
}

#wykonanie algorytmu genetycznego z podanymi parametrami i zwrócenie uśrednienionych wyników
logResults <- function(popSize, pmutation, pcrossover, tsp, tsp_range) {
  fitnessValue <- getAverageResult(popSize, pmutation, pcrossover, tsp, tsp_range)
  print(paste("popSize: ", popSize, ", pmutation: ", pmutation,
              ", pcrossover: ", pcrossover, ", fitnessValue: ", fitnessValue))
  cat("\n")
  return(paste(popSize, pmutation, pcrossover, fitnessValue, sep = " "))
}

  #wykonanie badań dla zadanej funkcji w podanym przedziale
  runTests <- function(tsp, filename, tsp_range) {
    #zbiór parametrów do badań
    mutations <- c(0, 0.2, 0.4, 0.6, 0.8, 1)
    crossovers <- c(0, 0.2, 0.4, 0.6, 0.8, 1)
    populations <- c(10, 20, 50, 100, 200, 500)
    
    #domyślne parametry dla algorytmu Genetycznego
    popSizeDefault <- 50
    pmutationDefault <- 0.1
    pcrossoverDefault <- 0.8
    
    csv <- paste("popSize", "pmutation", "pcrossover", "fitnessValue", sep = " ")
  
    #przeprowadzenie badań dla zmiennej wartości prawdopodobieństwa mutacji
    for(i in 1:length(mutations)) {
      row <- logResults(popSizeDefault, mutations[i], pcrossoverDefault, tsp, tsp_range)
      #zapisanie otrzymanego wyniku wraz z parametrami
      csv <- paste(csv, row, sep = "\n")
    }

    
    #przeprowadzenie badań dla zmiennej wartości prawdopodobieństwa krzyżowania
    print("***CROSSOVER***")
    for(i in 1:length(crossovers)) {
      row <- logResults(popSizeDefault, pmutationDefault, crossovers[i], tsp, tsp_range)
      cat("\n")
      #zapisanie otrzymanego wyniku wraz z parametrami
      csv <- paste(csv, row, sep = "\n")
    }
    
    
    #przeprowadzenie badań dla zmiennej wielkości populacji
    print("***POPSIZE***")
    for(i in 1:length(populations)) {
      row <- logResults(populations[i], pmutationDefault, pcrossoverDefault, tsp, tsp_range)
      cat("\n")
      #zapisanie otrzymanego wyniku wraz z parametrami
      csv <- paste(csv, row, sep = "\n")
    }
    
    write.csv(csv, file = paste(filename, ".csv", sep = ""))
  }
  
  #wykonanie badań dla wybranych funkcji
  #tsp <- read_TSPLIB("gr24.tsp")
  #tsp_range <- c(1, 24)
  #runTests(tsp, "_gr24", tsp_range)
  
  #tsp <- read_TSPLIB("fri26.tsp")
  #tsp_range <- c(1, 26)
  #runTests(tsp, "_fri26", tsp_range)
  
  tsp <- read_TSPLIB("bayg29.tsp")
  tsp_range <- c(1, 29)
  runTests(tsp, "_bayg29", tsp_range)
  
  