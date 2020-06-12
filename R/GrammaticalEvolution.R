GrammaticalEvolution <-  function(grammarDef, evalFunc, 
                                  numExpr = 1, 
                                  max.depth = GrammarGetDepth(grammarDef),
                                  startSymb = GrammarStartSymbol(grammarDef),
                                  seqLen = GrammarMaxSequenceLen(grammarDef, max.depth, startSymb),
                                  wrappings=3, 
                                  suggestions=NULL,
                                  optimizer = c("auto", "es", "ga"),
                                  popSize = "auto", newPerGen = "auto", elitism = 2,
                                  mutationChance=NA,
                                  iterations="auto",
                                  terminationCost=NA,
                                  monitorFunc=NULL,
                                  disable.warnings=FALSE,
                                  plapply=lapply, ...){
  
  if (numExpr < 1) {
    stop("Number of Expressions (numExpr) has to be at least 1.");
  }
  
  # prepare chromosome cutting indices
  chromosomeLen <- seqLen * numExpr
  
  # determine the values that are not given
  optimizer <- match.arg(optimizer)
  
  if (optimizer == "auto") {
    # set the optimiser
    if (numExpr > 1) {
      optimizer = "ga"
    } else {
      optimizer = "es"
    }
  }

  # automatically set the population size
  if (popSize == "auto") {
    if (optimizer == "ga") {
      popSize = 200
    } else {
      popSize = 8
    }
  }
  
  # automatically set GA iterations (generations) number
  if (iterations == "auto") {
    iterations = 1000

    # minimize number of iterations for smaller grammars
    num.grammar.expr = GrammarNumOfExpressions(grammarDef, max.depth, startSymb)
    iterations = round(min(num.grammar.expr / popSize * 2,
                           iterations))

    # as GA population is higher, reduce number of generations
    if (optimizer == "ga") { 
      iterations = round(iterations/5)
    }
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
  
  if (is.na(mutationChance)) {
    if (optimizer == "es") {
      mutationChance <- min(0.1, 5 / (1 + chromosomeLen))  
    } else {
      mutationChance <- 1 / (1 + chromosomeLen)
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
      tryCatch({  # use try-catch to catch failed parses
        expr <- GrammarMap(ch, grammarDef, wrappings = wrappings)
        if (expr$type == "T") {
          expr.list <- c(expr.list, as.expression(expr))
        }
      }, warning = function(w) print(w), error = function(e) print(e))
    }
    
    return(expr.list)
  }
  
  # evaluate the chromosome
  ga.evalFunc <- function(chromosome) {
    
    # convert multiple chromosomes to expression list
    expr.list = chromToExprList(chromosome)
    
    # check for empty expression list
    if (length(expr.list) == 0) {
      return (Inf)  # return very high error if all data is non-terminal
    }
    
    if (disable.warnings) {
      eval.results = suppressWarnings(evalFunc(expr.list))
    } else {
      eval.results = evalFunc(expr.list)
    }
    
    # evaluate the expressions
    return (eval.results)
  }
  
  add.expression.to.results <- function(ga.result) {
    ga.result$best$expressions = chromToExprList(ga.result$best$genome)
    class(ga.result) <- "GrammaticalEvolution"
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
                             evalFunc=ga.evalFunc,
                             suggestions=suggestions,
                             popSize=popSize, iterations=iterations, elitism=elitism, mutationChance=mutationChance,
                             geneCrossoverPoints = geneCrossoverPoints,
                             terminationCost=terminationCost,
                             monitorFunc=ga.monFunc,
                             allowrepeat = TRUE,
                             plapply=plapply, ...)
  } else {
    result <- EvolutionStrategy.int(genomeLen=chromosomeLen, 
                                    codonMin = 0, codonMax = GrammarMaxRuleSize(grammarDef) - 1,
                                    evalFunc=ga.evalFunc,
                                    suggestion=suggestions,
                                    mutationChance=mutationChance,
                                    popSize=popSize, newPerGen = newPerGen,
                                    iterations=iterations, terminationCost=terminationCost,
                                    monitorFunc=ga.monFunc,
                                    allowrepeat = TRUE,
                                    plapply=plapply, ...)
  }
  
  
  return (add.expression.to.results(result))
}

