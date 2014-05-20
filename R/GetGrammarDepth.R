GetGrammarDepth <-
function(grammar, 
    max.depth = length(grammar$def), 
    startSymb = grammar$startSymb) {
# returns the maximum depth of grammatical tree

  if (IsGrammarTerminal(startSymb) || (max.depth == 0))
    return (0)
    
  TERMINAL = TRUE
  NON_TERMINAL = FALSE

  depth = 0
  while(TRUE) {
    sep.symbs = GetFirstNonTerminalandRest(startSymb)
    terminal.symb = sep.symbs[[1]]
    non.terminal.symb = sep.symbs[[2]]
    rest = sep.symbs[[3]]
    possible.choices = GetPossibleRuleChoices(non.terminal.symb, grammar)

    depth_list = sapply(1:possible.choices, function(choice.no) {
      chosen.rule = ChosenGrammarRule(non.terminal.symb, choice.no, grammar)
      GetGrammarDepth(grammar, max.depth - 1, startSymb = chosen.rule)
    })
    depth = max(depth, depth_list)

    if (IsGrammarTerminal(rest)) {
        break
    } else {
        startSymb = rest
    }
  }

  return (1 + depth)
}
