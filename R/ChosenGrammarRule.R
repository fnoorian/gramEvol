ChosenGrammarRule <-
function(rule.name, choice, grammar.rules){
  
  rule.index = GetGrammarRuleIndex(grammar.rules, rule.name)
  rule.to.apply = grammar.rules$def[[rule.index]][[2]][[choice]]
  return(rule.to.apply)
}
