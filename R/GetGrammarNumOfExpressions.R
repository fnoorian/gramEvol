GetGrammarNumOfExpressions <-
function(grammar, 
    max.depth = length(grammar$def), 
    startSymb = grammar$startSymb) {
# returns the maximum number of expressions generatable by grammar
# max.depth is the maximum depth to search into the tree. It is used to avoid
# looping in recursive grammar

  if (IsGrammarTerminal(startSymb))
    return (1)

  if (max.depth == 0)
    return (0)
    
  TERMINAL = TRUE
  NON_TERMINAL = FALSE

  depth = 1
  while(TRUE) {
    sep.symbs = GetFirstNonTerminalandRest(startSymb)
    terminal.symb = sep.symbs[[1]]
    non.terminal.symb = sep.symbs[[2]]
    rest = sep.symbs[[3]]
    possible.choices = GetPossibleRuleChoices(non.terminal.symb, grammar)

    depth_list = sapply(1:possible.choices, function(choice.no) {
      chosen.rule = ChosenGrammarRule(non.terminal.symb, choice.no, grammar)
      GetGrammarNumOfExpressions(grammar, max.depth - 1, startSymb = chosen.rule)
    })
    depth = depth * sum(depth_list)

    if (IsGrammarTerminal(rest)) {
        break
    } else {
        startSymb = rest
    }
  }

  return (depth)
}

