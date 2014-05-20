GetGrammarRuleIndex <-
function(grammar, rule.name) {
  # finds the index of rule in the grammar
  attr(grammar$defIndex, rule.name)
}
