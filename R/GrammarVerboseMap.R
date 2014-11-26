GrammarVerboseMap <- function(inputString, grammar, wrappings = 3) {
  
  if (class(grammar) != "grammar") {
    stop("Invalid Grammar Class")
  }
  
  startSymb <- grammar$startSymb # start symbol
  
  for (i in 1:wrappings) {
    
    if (i > 1) {
      cat("Wrapping string to position 0\n")
    }
    
    codon.string = inputString
    
    total.genes = length(codon.string)
    
    cat('         Starting:', startSymb, '\n')
    for (j in 1:total.genes){
      res = ApplyGrammarRule(codon.string[j], startSymb, grammar)
      cat('Codon: ', codon.string[j], '\tResult:', res, '\n')
      if (res == startSymb){ # if not replacement is performed, we have reached a terminal
        break
      }
      startSymb = res  
    }
    
    result.string = startSymb
    
    if (IsSymbolTerminal(result.string)) {
      cat("Valid Expression Found\n")
      #cat(as.character(parse(text=unescape.gt.lt(result.string))), '\n')
      break
    } else {
      cat("Non-terminal expression\n")
      exprs = list(expr = result.string, type = "NT", parsed = NULL)
    }
  }  
}
