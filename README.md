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

### Usage

A tutorial on implementing GE programs is included in the package's
[vignette](https://cran.r-project.org/web/packages/gramEvol/vignettes/ge-intro.pdf).

More information regarding GE and its application in parameter optimization is brought in
[this paper in Journal of statistical Software](https://www.jstatsoft.org/article/view/v071i01).

### Contact Information
 * Farzad Noorian <farzad.noorian@gmail.com> (Maintainer)
 * Anthony Mihirana de Silva <mihids@gmail.com>

### Release and Development
The latest release and developmental versions of this package are available on:
<https://github.com/fnoorian/gramEvol>
 
### License
All files in this package, including the documentation and vignettes,
are distributed under GNU GPL v2.0 or later license.
For full terms of this license visit <https://www.gnu.org/licenses/gpl-2.0.html>.

