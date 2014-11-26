summary.grammar <- function(object, ...) {
  cat("Start Symbol:               ", GrammarStartSymbol(object), '\n')
  cat("Is Recursive:               ", GrammarIsRecursive(object), '\n')
  cat("Tree Depth:                 ", GrammarGetDepth(object), '\n')
  cat("Maximum Sequence Length:    ", GrammarMaxSequenceLen(object), '\n')
  cat("Maximum Rule Size:          ", GrammarMaxRuleSize(object), '\n')
  cat("Maximum Sequence Variation: ", GrammarMaxSequenceRange(object), '\n')
  cat("No. of Unique Expressions:  ", GrammarNumOfExpressions(object), '\n')
}
