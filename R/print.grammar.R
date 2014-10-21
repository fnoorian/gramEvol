print.grammar <- function (x, ...) {
  maxLen = max(sapply (x$def, function(rule) nchar(rule[[1]])))
  
  for (rule in x$def) {
    spaces = maxLen - nchar(rule[[1]])  # use spaces as tabs usually don't work
    expressions = lapply(rule[[2]], function(x) x)
    cat(paste0("<", rule[[1]], ">" ), 
        do.call(paste0, as.list(rep(" ", spaces))),
        " ::= ",
        do.call(function(...) {paste(..., sep=" | ")}, expressions), 
        "\n", ..., sep = "")
  }
}
