GrammaticalEvolution <-
function(grammarDef, fitnessFunction, 
  seqLen = GetGrammarMaxSequenceLen(grammarDef), wrappings=3,
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
    mutationChance = 1 / (1 + chromosomeLen)
  }

  if (numExpr == 1) {
    ind.cut <- 1
  } else {
    ind.cut <- as.numeric(cut(1:chromosomeLen, numExpr))
  }

  # divide chromosome into N parts and convert them to expressions
  chromToExprList <- function(chromosome) {

    expr.list = c()
    for (i in 1:numExpr) {
      ch <- chromosome[ind.cut == i]
      expr <- GrammarGenotypeToPhenotype(ch, grammarDef, wrappings = wrappings)
  
      if (expr$type == "T") {
        expr.list <- c(expr.list, expr$expr)
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

  if (!is.null(monitorFunc)) {
    # report by adding the best expressions
    ga.monFunc <- function(result) {
      res = list(bestExpression = chromToExprList(result$bestChrom),
               gaSummary = result)
      monitorFunc(res)  
    }
  } else {
    ga.monFunc <- NULL
  }

  ga.result <- GeneticAlg.int(genomeLen=chromosomeLen, 
    codonMin = 0, codonMax = grammarDef$maxRuleSize - 1,
    evalFunc=evalFunc,
    suggestions=suggestions,
    popSize=popSize, iterations=iterations, elitism=elitism, mutationChance=mutationChance,
    terminationFitness=terminationFitness,
    monitorFunc=ga.monFunc,
    allowrepeat = TRUE,
    plapply=plapply, ...)

  return (list(bestExpression = chromToExprList(ga.result$bestChrom),
               gaSummary = ga.result))
}
