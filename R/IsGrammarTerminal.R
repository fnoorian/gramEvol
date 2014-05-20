IsGrammarTerminal <-
function(symb){
  # checks if the grammar is terminal or not
  grep.res.start = gregexpr("<", symb)[[1]]
  return (grep.res.start[1] == -1)
}
