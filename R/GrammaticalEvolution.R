GrammaticalEvolution <-  
  function(grammarDef, fitnessFunction, 
           seqLen = GrammarMaxSequenceLen(grammarDef), wrappings=3,
           elitism=2, popSize=50, iterations=100, terminationFitness=NA, 
           mutationChance=NA,
           numExpr = 1, 
           suggestions=NULL,
           monitorFunc=NULL,
           plapply=lapply, ...) {
    
  if (numExpr < 1) {
    stop("Number of Expressions (numExpr) has to be at least 1.");
  }
  
  # prepare chromosome cutting indices
  chromosomeLen <- seqLen * numExpr
  
  if (is.na(mutationChance)) {
    mutationChance <- 1 / (1 + chromosomeLen)
  }
  
  if (numExpr == 1) {
    ind.cut <- 1
    geneCrossoverPoints <- NULL
  } else {
    ind.cut <- as.numeric(cut(1:chromosomeLen, numExpr))
    geneCrossoverPoints <- ind.cut
  }
  
  # divide chromosome into N parts and convert them to expressions
  chromToExprList <- function(chromosome) {
    
    expr.list = c()
    for (i in 1:numExpr) {
      ch <- chromosome[ind.cut == i]
      expr <- GrammarMap(ch, grammarDef, wrappings = wrappings)
      
      if (expr$type == "T") {
        expr.list <- c(expr.list, as.expression(expr))
      }
    }
    
    return(expr.list)
  }
  
  # evaluate the chromosome
  evalFunc <- function(chromosome) {
    
    # convert multiple chromosomes to expression list
    expr.list = chromToExprList(chromosome)
    
    # check for empty expression list
    if (length(expr.list) == 0) {
      return (Inf)  # return very high error if all data is non-terminal
    }
    
    # evaluate the expressions
    return (fitnessFunction(expr.list))
  }

  collect.results <- function(ga.result) {
    ga.result$best$expressions = chromToExprList(ga.result$best$genome)
    class(ga.result) <- "GramEvol"
    return(ga.result)
  }
  
  if (!is.null(monitorFunc)) {
    # report by adding the best expressions
    ga.monFunc <- function(result) {
      result$best$expressions = chromToExprList(result$best$genome)
      class(result) <- "GramEvol"
      monitorFunc(result)  
    }
  } else {
    ga.monFunc <- NULL
  }
  
  ga.result <- GeneticAlg.int(genomeLen=chromosomeLen, 
                              codonMin = 0, codonMax = grammarDef$maxRuleSize - 1,
                              evalFunc=evalFunc,
                              suggestions=suggestions,
                              popSize=popSize, iterations=iterations, elitism=elitism, mutationChance=mutationChance,
                              geneCrossoverPoints = geneCrossoverPoints,
                              terminationFitness=terminationFitness,
                              monitorFunc=ga.monFunc,
                              allowrepeat = TRUE,
                              plapply=plapply, ...)
  
  return (collect.results(ga.result))
}
