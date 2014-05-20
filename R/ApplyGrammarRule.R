ApplyGrammarRule <-
function(current.codon, startSymb, grammar){

  TERMINAL = TRUE
  NON_TERMINAL = FALSE

  sep.symbs = GetFirstNonTerminalandRest(startSymb)
  if (sep.symbs[[1]] == NON_TERMINAL){
    return("terminal")
  }
  terminal.symb = sep.symbs[[1]]
  non.terminal.symb = sep.symbs[[2]]
  rest = sep.symbs[[3]]
  possible.choices = GetPossibleRuleChoices(non.terminal.symb, grammar)
  choice.no = current.codon %% possible.choices
  chosen.rule = ChosenGrammarRule(non.terminal.symb, choice.no+1, grammar)
  new.startSymb = paste0(terminal.symb, chosen.rule, rest)
  new.startSymb
}
