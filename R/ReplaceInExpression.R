#' Replace as sub-expression isnide an expression
#'
#' Replace every subexpression equal to or starting with \code{what} in \code{expr}.
#' Replacement is performed by passing the whole subexpression to \code{replacer.func},
#' which should be a \code{function(x, ...)}, where \code{x} is the expression and return the desirable expression.
#'
#' @details This function was designed to be used as a runtime processing tool for
#' grammar generated expression. This allows the user to modify the resulting
#' expression on the fly based on runtime variables, without including them
#' in the grammar. See examples section.
#'
#' @param expr           An \code{\link{expression}}.
#' @param what           A backquoted expression to find in \code{expr}.
#' @param replacer.func  A \code{function(x, ...)} to process the subexpression.
#' @param ...            Other parameters passed to \code{replacer.func}.
#' @return  An expression
#'
#' @references See \url{http://adv-r.had.co.nz/Expressions.html} by Hadley Wickham.
#'
#' @examples
#' expr = expression(function(x) {
#'     cbind(f1(x),
#'           f2(x),
#'           g3(y))
#' })
#' expr
#' ReplaceInExpression(expr, bquote(f2), function(x) {NULL})
#' ReplaceInExpression(expr, bquote(f2), function(x) {bquote(f2(y))})
#' ReplaceInExpression(expr, bquote(g3), function(x) {bquote(f3(x))})
#' ReplaceInExpression(expr, bquote(g3), function(x, b) {if (b > 1) x else NULL}, b = 0)
#' ReplaceInExpression(expr, bquote(g3), function(x, b) {if (b > 1) x else NULL}, b = 2)

ReplaceInExpression <- function (expr, what, replacer.func, ...) {

  if (is.expression(expr)) {
    as.expression(
      ReplaceInExpression(as.call(expr),
                          what = what, replacer.func = replacer.func, ...)[[1]]
    )
  } else if (is.atomic(expr) || is.name(expr)) {
    if (identical(expr, what)) {
      replacer.func(expr, ...)
    } else {
      # Leave unchanged
      expr
    }
  } else if (is.call(expr)) {
    if (identical(expr[[1]], what)) {
      # replace it using your given function
      replacer.func(expr, ...)
    } else {
      # Otherwise apply recursively, turning result back into call
      as.call(lapply(expr, ReplaceInExpression, what = what, replacer.func = replacer.func, ...))
    }
  } else if (is.pairlist(expr)) {
    as.pairlist(lapply(expr, ReplaceInExpression, what = what, replacer.func = replacer.func, ...))
  } else {
    # User supplied incorrect input
    stop("Don't know how to handle type ", typeof(expr),
         call. = FALSE)
  }
}

