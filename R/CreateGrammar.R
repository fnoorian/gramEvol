CreateGrammar <-
function(ruleDef, startSymb = "<expr>") {

  # if the ruledef is a file, read it
  if (("connection" %in% class(ruleDef)) | is.character(ruleDef)) {
    ruleDef = ReadBNFFile(ruleDef)
  }

  # trim brackets from the rule index
  trim_brackets <- function (x) gsub("^<+|>+$", "", x)
  for (i in seq_along(ruleDef)) {
    ruleDef[[i]][[1]] = trim_brackets(ruleDef[[i]][[1]])
  }

  # extract information about the grammar
  ruleSizes = sapply(ruleDef, function(r) length(r[[2]]))

  ruleDefIndex = sapply(ruleDef, function(r) r[[1]])

  for (i in 1:length(ruleDef))
    attr(ruleDefIndex, ruleDef[[i]][[1]]) = i

  grammar = list(def = ruleDef,             # Grammar in List Format
               defIndex = ruleDefIndex,     # Index of each Rule
               startSymb = startSymb,       # start Symbol
               ruleSizes = ruleSizes,       # Length of each Rule as attr
               maxRuleSize = max(ruleSizes))# maximum choice in a rule

  class(grammar) = "grammar"

  return (grammar)
}
