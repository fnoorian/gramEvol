TraverseCodon <-
function(codon.string, startSymb, grammar){
  
  total.genes = length(codon.string)

  for (i in 1:total.genes){
    res = ApplyGrammarRule(codon.string[i], startSymb, grammar)
    if (res == "terminal"){
      return(startSymb) 
    }
    startSymb = res  
  }
  startSymb
}
