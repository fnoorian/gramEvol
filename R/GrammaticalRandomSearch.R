GrammaticalRandomSearch <- function(grammar, evalFunc,
                                max.depth = GrammarGetDepth(grammar),
                                startSymb = GrammarStartSymbol(grammar),
                                wrappings = 3,
                                iterations = 1000, terminationFitness = NA,
                                monitorFunc = NULL) {
  
  # determine depth of search
  upper = GrammarMaxSequenceRange(grammar, max.depth, startSymb)
  n = length(upper) # size of chromosome
  
  # the list of chromosomes
  genomes = NULL
  scores = NULL
  
  # do the search
  best.genome = NULL
  best.score = Inf
  best.expr = NULL
  for (i in 1:iterations) {
    genome = round(runif(n) * upper)
    
    expr = GrammarMap(genome, grammar, wrappings)
    if (expr$type == "NT") {
      score = Inf
    } else {
      score = evalFunc(as.expression(expr))
    }
    
    genomes = rbind(genomes, genome)
    scores = rbind(scores, score)
    
    if (score < best.score) {
      best.score = score
      best.genome = genome
      best.expr = expr
      
      if (!is.na(terminationFitness)) {
        if (score < terminationFitness) {
          break        
        }
      }
    }
      
    if (!is.null(monitorFunc)) {
      res <- list(population = genomes,
                  populationFitness = scores,
                  currentSequence = genome,
                  currentExpression = expr,
                  currentScore = score,
                  bestSequence = best.genome,
                  bestExpression = best.expr,
                  bestScore = best.score,
                  numExpr = i)

      class(res) <- "GESearch"
      monitorFunc(res)
    }
  }
  
  res <- list(population = genomes,
              populationFitness = scores,
              bestSequence = best.genome,
              bestExpression = best.expr,
              bestFitness = best.score,
              numExpr = i)

  class(res) <- "GESearch"
  return (res)
}

