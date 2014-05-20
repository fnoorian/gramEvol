GrammarGenotypeToPhenotype <-
function(inputString, grammar, wrappings = 3) {
# Integer String to Expression using Grammar

  if (class(grammar) != "grammar") {
    stop("Invalid Grammar Class")
  }

  result.string <- grammar$startSymb # start symbol

  for (i in 1:wrappings) {
    result.string = TraverseCodon(inputString, result.string, grammar)  
    
    if (IsGrammarTerminal(result.string)) {
      exprs = list(expr = result.string, type = "T")
      break
    } else {
      exprs = list(expr = result.string, type = "NT")
    }
  }  

  class(exprs) = "GEPhenotype"

  return (exprs)
}
