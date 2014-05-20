GetFirstNonTerminalandRest <-
function(rule){

  NON_TERMINAL = FALSE

  grep.res.start = gregexpr("<", rule)[[1]]
  if (grep.res.start[1] == -1){
    return(list(NON_TERMINAL))
  }
  grep.res.end = gregexpr(">", rule)[[1]]
  if (grep.res.end[1] == -1){
    stop("invalid rule representation")
  }
  terminals = substr(rule, 1, grep.res.start-1)
  first.nonterminal = substr(rule, grep.res.start+1, grep.res.end-1)
  rest = substr(rule, grep.res.end+1, nchar(rule))
  return(list(terminals, first.nonterminal, rest))

}
