[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/gramEvol)](https://cran.r-project.org/package=gramEvol)
[![Downloads](https://cranlogs.r-pkg.org/badges/gramEvol)](https://cran.r-project.org/package=gramEvol)

gramEvol: Grammatical Evolution for R
=====================================

This package includes source code and documentation of *gramEvol: Grammatical
Evolution for R*.

*gramEvol* implements grammatical evolution (GE) in native R syntax. It allows
discovering programs that can achieve a desired goal, by performing an
evolutionary optimization over a population of R expressions generated via a
user-defined grammar. Functions are provided for creating and manipulating
context-free grammars (CFGs), random search, exhaustive search, and evolutionary
optimization. Users are only required to define their program structure via a
grammar, and a cost function to evaluate the fitness of each program.

### Installation

You can install this package from CRAN:
```R
install.packages("gramEvol")
```

You can install the latest version from Github:
```R
if (!require("devtools")) install.packages("devtools")
devtools::install_github("fnoorian/gramEvol")
```

## Usage

A tutorial on implementing GE programs is included in the package's
[vignette](https://fnoorian.github.io/gramEvol/inst/doc/ge-intro.html)
([PDF version](https://fnoorian.github.io/gramEvol/inst/doc/ge-intro.pdf)).

More information regarding GE and its application in parameter optimization can be found in
[this paper in the Journal of Statistical Software](https://www.jstatsoft.org/article/view/v071i01).

## Example

This example implements the [Kepler law rediscovery](https://fnoorian.github.io/gramEvol/inst/doc/ge-intro.html#rediscovery-of-keplers-law-by-symbolic-regression) problem,
as discussed in section 3.1 of the vignette.

```R
library("gramEvol")

# grammar definition for generic symbolic regression
grammarDef <- CreateGrammar(list(
  expr  = grule(op(expr, expr), func(expr), var),
  func  = grule(sin, cos, log, sqrt),
  op    = grule(`+`, `-`, `*`), # define unary operators
  var   = grule(distance, distance^n, n),
  n     = gvrule(1:4) # this is shorthand for grule(1,2,3,4)
))

# cost function and data
planets <- c("Venus", "Earth", "Mars", "Jupiter", "Saturn", "Uranus")
distance <- c(0.72, 1.00, 1.52, 5.20, 9.53, 19.10)
period <- c(0.61, 1.00, 1.84, 11.90, 29.40, 83.50)

SymRegCostFunc <- function(expr) {
     result <- eval(expr)
  
       if (any(is.nan(result)))
           return(Inf)
  
       return (mean(log(1 + abs(period - result))))
}

# run GE
ge <- GrammaticalEvolution(grammarDef, SymRegCostFunc, iterations = 50)
print(ge)

# use the best expression
best.expression <- ge$best$expression
print(ge$best$expressions)
print(data.frame(distance, period, Kepler = sqrt(distance^3), GE = eval(best.expression)))
```

## Contact Information
 * Farzad Noorian <farzad.noorian@gmail.com> (Maintainer)
 * Anthony Mihirana de Silva <mihids@gmail.com>

## Release and Development
The latest release and developmental versions of this package are available on:
<https://github.com/fnoorian/gramEvol>
 
## License
All files in this package, including the documentation and vignettes,
are distributed under GNU GPL v2.0 or later license.
For full terms of this license visit <https://www.gnu.org/licenses/gpl-2.0.html>.

