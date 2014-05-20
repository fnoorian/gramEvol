GetGrammarMaxSequenceLen <-
function(grammar, 
    max.depth = GetGrammarDepth(grammar), 
    startSymb = grammar$startSymb) {
# returns the maximum length of a sequence required for the grammar expression
#    generation
# max.depth: the maximum depth to search into the tree. It is used to avoid
#    looping in recursive grammar

 if (IsGrammarTerminal(startSymb))
    return (0)
 if (max.depth == 0)
    return (-1)
    
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
      GetGrammarMaxSequenceLen(grammar, max.depth - 1, startSymb = chosen.rule)
    })

    # break out if have no valid children
    if (max(depth_list) == -1)
        return (-1)

    depth = depth + 1 + max(depth_list)

    if (IsGrammarTerminal(rest)) {
        break
    } else {
        startSymb = rest
    }
  }

  return (depth)
}

