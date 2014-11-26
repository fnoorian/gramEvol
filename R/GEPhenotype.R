as.character.GEPhenotype <- function(x, ...) {
  x$expr
}

as.expression.GEPhenotype <- function(x, ...) {
  if (x$type == "NT") {
    warning("Non-Terminal Phenotype")
    NULL
  } else {
    x$parsed
  }
}

print.GEPhenotype <- function(x, ..., simplify=TRUE) {
  if (x$type == "NT") {
    print("Non-Terminal Phenotype", ...)
    #print(NA, ...)
  } else {
    expr = x$parsed
    
    if (simplify) {
      expr = parse(text=as.character(expr))
    }
    print(expr, ...)
  }
}
