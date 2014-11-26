GrammarGetNextSequence <- 
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
  
  # find the starting sequence based on the given input
  fullStartSeq = GrammarGetFirstSequence(grammar, seqStart, startSymb, max.len)
  if (is.GrammarOverflow(fullStartSeq)) {
    return(fullStartSeq)
  }
  
  # if incomplete seqStart is given, simply return the full version
  if (length(seqStart) < length(fullStartSeq)) {
    return(fullStartSeq)
  }
  
  seqStart = fullStartSeq
  
  # find the possible choices
  possible.choices = GetPossibleRuleChoicesFirstSymbol(startSymb, grammar)
  
  # extract the first element
  if (length(seqStart) == 0) {
    seqStart = 0
  }
  current.codon = seqStart[1]
  restOfSequence = seqStart[-1]
  
  # if at the end of sequence, simply move to next codon
  if (length(restOfSequence) == 0) {
    current.codon = current.codon + 1
  }
  
  # check for overflow
  if (current.codon >= possible.choices) {
    return(NewGrammarOverFlow())
  }
  
  for (i in current.codon:(possible.choices-1)) {
    
    # apply the rule
    newSymb = ApplyGrammarRule(i, startSymb, grammar)
    
    # recursively apply for the next
    new_seq = GrammarGetNextSequence(grammar, seqStart = restOfSequence, startSymb = newSymb, max.len = max.len - 1)
    
    if (!is.GrammarOverflow(new_seq)) {
      new_seq = c(i, new_seq)
      break
    } else {
      restOfSequence = NULL
    }
  }
  
  return (new_seq)
}
