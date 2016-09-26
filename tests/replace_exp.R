library("gramEvol")

is.expr.eq <- function(x, y) {
  identical(as.character(x), as.character(y))
}


expr = expression(function(x) {
  cbind(f1(x),
        f2(x),
        g3(y))
})


r0 = ReplaceInExpression(expr, bquote(y), function(x) { bquote(x) })
stopifnot(is.expr.eq(r0, expression(function(x) { cbind(f1(x), f2(x), g3(x)) })))

r1 = ReplaceInExpression(expr, bquote(f2), function(x) {NULL})
stopifnot(is.expr.eq(r1, expression(function(x) { cbind(f1(x), NULL, g3(y)) })))

r2 = ReplaceInExpression(expr, bquote(f2), function(x) {bquote(f2(y))})
stopifnot(is.expr.eq(r2, expression(function(x) { cbind(f1(x), f2(y), g3(y)) })))

r3 = ReplaceInExpression(expr, bquote(g3), function(x) {bquote(f3(x))})
stopifnot(is.expr.eq(r3, expression(function(x) { cbind(f1(x), f2(x), f3(x)) })))


r4 = ReplaceInExpression(expr, bquote(g3), function(x, b) {if (b > 1) x else NULL}, b = 0)
stopifnot(is.expr.eq(r4, expression(function(x) { cbind(f1(x), f2(x), NULL) })))

r5 = ReplaceInExpression(expr, bquote(g3), function(x, b) {if (b > 1) x else NULL}, b = 3)
stopifnot(is.expr.eq(r5, expression(function(x) { cbind(f1(x), f2(x), g3(y)) })))
