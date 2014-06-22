EvalExpressions <- 
  function(expressions, envir = parent.frame()) {
  # evaluates one or a collection of expression to numerial value

  # if the input is an expression, make it in to a list
  if (class(expressions) == "GEPhenotype") {
    expressions = list(expressions)
  }

  # evaluate the expressions
  vals <- list()
  for (expr in expressions){

    # either use phenotype class
    if (class(expr) == "GEPhenotype") {
        if (expr$type == "T") {
            # extract the expression for terminal grammar
            expr = expr$expr
        } else {
            warning("Can not evaluate non-terminal expression")
            expr = "NA"
        }
    # or evaluate character string
    } else if (class(expr) != "character") {
        warning("Can not evaluate invalid expression")
        expr = "NA"
    }

    result <- eval(parse(text = expr), envir=envir)
    vals <- c(vals, list(result))
  }

  # return a vector if a single expression
  if (length(expressions) == 1)
    return (vals[[1]])

  # convert list to dataframe and set column names
  vals = do.call(data.frame, vals)
  colnames(vals) = paste0("expr", seq_along(expressions))

  return (vals)
}
