print.grammar <-
function(x, ...) {
  
  for (rule in x$def) {
    cat(paste0('<', rule[[1]], ">:"), unlist(lapply(rule[[2]], function(x) paste0('"', x, '"'))), "\n", ...)
  }
}


