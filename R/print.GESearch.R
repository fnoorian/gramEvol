print.GESearch <- function (x, ..., max.line.len = 60) {
  
  cat("GE Search Results:\n")
  cat("  Expressions Tested:", x$numExpr, '\n')
  cat("  Best Expression:", as.character(x$bestExpression), '\n')
  cat("  Best Fitness:", x$bestFitness, '\n')
}

