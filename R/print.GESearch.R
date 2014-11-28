print.GESearch <- function (x, ..., max.line.len = 60) {
  
  cat("GE Search Results:\n")
  cat("  Expressions Tested:", x$numExpr, '\n')
  cat("  Best Expression:", as.character(x$bestExpression), '\n')
  cat("  Best Fitness:", x$bestFitness, '\n')
}

print.GeneticAlg.int <- function(x, ...) {
  cat("Genetic Algorithm Search Results:\n")
  
  cat("  No. Generations:", as.character(x$population$currentIteration), "\n")
  #cat("  No. Evaluations:", as.character(x$population$currentIteration * x$settings$totalPopulation), "\n")
  cat("  Best Fitness:   ", as.character(x$best$fitness), "\n")
  cat("  Best Genome:", as.character(x$best$genome), "\n")
}

print.EvolutionStrategy.int <- function(x, ...) {
  cat("Evolution Strategy Search Results:\n")
  
  cat("  No. Generations:", as.character(x$population$currentIteration), "\n")
  #cat("  No. Evaluations:", as.character(x$population$currentIteration * x$settings$totalPopulation), "\n")
  cat("  Best Fitness:   ", as.character(x$best$fitness), "\n")
  cat("  Best Genome:", as.character(x$best$genome), "\n")
}

print.GrammaticalEvolution <- function(x, ...) {
  cat("Grammatical Evolution Search Results:\n")
  
  cat("  No. Generations:", as.character(x$population$currentIteration), "\n")
  #cat("  No. Evaluations:", as.character(x$population$currentIteration * x$settings$totalPopulation), "\n")
  cat("  Best Fitness:   ", as.character(x$best$fitness), "\n")
  cat("  Best Expression:", as.character(x$best$expressions), "\n")
}
