GrammarGetFirstSequence <- 
  function (grammar, 
            seqStart = NULL, 
            startSymb = GrammarStartSymbol(grammar), 
            max.len = GrammarMaxSequenceLen(grammar)) {
  
  if (IsSymbolTerminal(startSymb)) {
    return (NULL)
  }
  if (max.len == 0) {
    return (NewGrammarOverFlow())
  }
  
  # find the possible choices
  possible.choices = GetPossibleRuleChoicesFirstSymbol(startSymb, grammar)
  
  # extract the first element
  if (length(seqStart) == 0) {
    seqStart = 0
  }
  current.codon = seqStart[1]
  restOfSequence = seqStart[-1]
  
  # check for overflow
  if (current.codon >= possible.choices) {
    return(NewGrammarOverFlow())
  }
  
  for (i in current.codon:(possible.choices-1)) {
    # apply the rule
    newSymb = ApplyGrammarRule(i, startSymb, grammar)
    
    # recursively apply for the next
    new_seq = GrammarGetFirstSequence(grammar, seqStart = restOfSequence, startSymb = newSymb, max.len = max.len - 1)
    
    if (!is.GrammarOverflow(new_seq)) {
      new_seq = c(i, new_seq)
      break
    }
  }
  
  return (new_seq)
}
