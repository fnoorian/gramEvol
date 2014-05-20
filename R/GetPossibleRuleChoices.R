GetPossibleRuleChoices <-
function(non.terminal.symb, grammar.rules){
  if (!is.character(non.terminal.symb))
    stop("Invalid non-string input to GetPossibleRuleChoices()")
  ruleInd = GetGrammarRuleIndex(grammar.rules, paste(non.terminal.symb))
  return(grammar.rules$ruleSizes[ruleInd])
}
