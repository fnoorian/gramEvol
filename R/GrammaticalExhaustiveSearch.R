GrammaticalExhaustiveSearch <- function(grammar, evalFunc,
                                        max.depth = length(grammar$def), 
                                        startSymb = GrammarStartSymbol(startSymb),
                                        wrappings = 3,
                                        terminationFitness = NA,
                                        monitor = NULL) {
  
  best.expr = NULL
  best.score = Inf
  best.seq = NULL
  
  iterations = 0
  genome = NULL
  while (TRUE) {
    genome = GrammarGetNextSequence(grammar, genome)
    
    if (is.GrammarOverflow(genome))
      break
    
    iterations = iterations + 1
    
    expr = as.expression(GrammarGenotypeToPhenotype(genome, grammar, wrappings))
    
    score = evalFunc(expr)
    
    if (score < best.score) {
      best.score = score
      best.expr = expr
      best.seq = genome
      if (!is.na(terminationFitness)) {
        if (score < terminationFitness) {
          break        
        }
      }
    }
    
    if (!is.null(monitor)) {
      monitor(list(currentSequence = genome,
                   currentExpression = expr,
                   currentScore = score,
                   bestSequence = best.seq,
                   bestExpression = best.expr,
                   bestScore = best.score,
                   iter = iterations))
    }
  }
  
  return(list( bestSequence = best.seq,
               bestExpression = best.expr,
               bestScore = best.score,
               iter = iterations))
}
