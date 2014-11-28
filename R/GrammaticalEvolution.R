GrammaticalEvolution <-  function(grammarDef, fitnessFunction, 
                                  numExpr = 1, 
                                  max.depth = GrammarGetDepth(grammarDef),
                                  startSymb = GrammarStartSymbol(grammarDef),
                                  seqLen = GrammarMaxSequenceLen(grammarDef, max.depth, startSymb),
                                  wrappings=3, 
                                  suggestions=NULL,
                                  optimizer = c("es", "ga"),
                                  popSize=8, newPerGen = "auto", elitism = 2,
                                  mutationChance=NA,
                                  iterations=1000, terminationFitness=NA, 
                                  monitorFunc=NULL,
                                  plapply=lapply, ...){
    
  if (numExpr < 1) {
    stop("Number of Expressions (numExpr) has to be at least 1.");
  }
  
  # prepare chromosome cutting indices
  chromosomeLen <- seqLen * numExpr
  
  # determine the values that are not given
  optimizer <- match.arg(optimizer)

  if (is.na(mutationChance)) {
    mutationChance <- 10 / (1 + chromosomeLen)
  }

  if (optimizer == "es" && newPerGen == "auto") {
    if (GrammarIsRecursive(grammarDef)) {
      # random search for recursive grammar
      newPerGen = popSize
      popSize = 0
    } else {
      # mixed search for non-recursive
      newPerGen = round(popSize / 4)
      popSize = popSize - newPerGen
    }
  }
  
  # determine the indicies for cutting chromosomes to N expressions
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

  add.expression.to.results <- function(ga.result) {
    ga.result$best$expressions = chromToExprList(ga.result$best$genome)
    class(ga.result) <- "GramEvol"
    return(ga.result)
  }
  
  if (!is.null(monitorFunc)) {
    # report by adding the best expressions
    ga.monFunc <- function(result) {
      monitorFunc(add.expression.to.results(result))  
    }
  } else {
    ga.monFunc <- NULL
  }
  
  if (optimizer == "ga") {
    result <- GeneticAlg.int(genomeLen=chromosomeLen, 
                                 codonMin = 0, codonMax = GrammarMaxRuleSize(grammarDef) - 1,
                                 evalFunc=evalFunc,
                                 suggestions=suggestions,
                                 popSize=popSize, iterations=iterations, elitism=elitism, mutationChance=mutationChance,
                                 geneCrossoverPoints = geneCrossoverPoints,
                                 terminationFitness=terminationFitness,
                                 monitorFunc=ga.monFunc,
                                 allowrepeat = TRUE,
                                 plapply=plapply, ...)
  } else {
    result <- EvolutionStrategy.int(genomeLen=chromosomeLen, 
                                    codonMin = 0, codonMax = GrammarMaxRuleSize(grammarDef) - 1,
                                    evalFunc=evalFunc,
                                    suggestion=suggestions,
                                    mutationChance=mutationChance,
                                    popSize=popSize, newPerGen = newPerGen,
                                    iterations=iterations, terminationFitness=terminationFitness,
                                    monitorFunc=ga.monFunc,
                                    allowrepeat = TRUE,
                                    plapply=plapply, ...)
  }

  
  return (add.expression.to.results(result))
}
