print.grammar <- function (x, ...) {
  maxLen = max(sapply (x$def, function(rule) nchar(rule[[1]])))
  
  for (rule in x$def) {
    spaces = maxLen + 1 - nchar(rule[[1]])  # use spaces as tabs usually don't work
    cat(paste0("<", rule[[1]], ">:"), 
        do.call(paste0, as.list(rep(" ", spaces))),
        unlist(lapply(rule[[2]], function(x) paste0("\"", x, "\""))), 
        "\n", ...)
  }
}


