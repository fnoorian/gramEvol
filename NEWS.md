# News

## 2017-XX-XX - Changes in version 2.1-4
 * Add ReplaceInExpression helper function
 * Update GeneticAlg.int to accept suggestions as a list (Thanks to Rafael Klaic)
 * Update vignette with new grammar rules
 * Fix bug in Evolutionary strategy when popsize = 1 (Thanks to Daniel Klotz <https://github.com/danklotz>)
 * Change ga.mutation parameter to fix its typo (Thanks to Daniel Klotz <https://github.com/danklotz>)

## 2016-07-04 - Changes in version 2.1-3

 * Add a supress warnings option to `GrammaticalEvolution` optimizer
 * Update citation with the information from Journal of Statistical Software
 * Fix iteration parameter auto-set overriding manual value bug
 * Fix suggested initial chromosome bug (Thanks to Daniel Klotz <https://github.com/danklotz>)

## 2015-11-11 - Changes in version 2.1-2

 * Changed contact email
 * Added citation information
 * Fixed global function imports in package namespace

## 2015-06-04 - Changes in version 2.1-1

 * Changed package's description, explaining what grammatical evolution is.

## 2015-06-03 - Changes in version 2.1-0

 * Added grammar rule concatenation
 * Added iterative grammar rule
 * Improved primitive function printing in the grammar
 * Fixed typos and cleaned up latex in the vignette
 * R-devel CRAN check compatibility fixes

## 2015-02-11 - Changes in version 2.0-2

 * R-devel CRAN check compatibility fix

## 2015-02-10 - Changes in version 2.0-1

 * New Vignettes
 
## 2014-12-09 - Changes in version 2.0-0

 * CreateGrammar accepts Expressions as well as strings
 * Added predefined multi-point crossover operator to Genetic Algorithm Implementation
 * New functions for iterating through the grammar
 * New Grammatical Exhaustive and Random Search Functions
 * New verbose functions to test expressions and grammatical generation
 * New print and summary overload for many objects
 
## 2014-10-21 - Changes in version 1.2-1

 * Added BNF file parser
 * CreateGrammar default value fixed
 * Brackets around grammar rule identifiers are optional now
 * Minor style improvements

##2014-06-23 - Changes in version 1.1-1

 * Added Some regression tests
 * Changed GA and GE default popsize and itercount to be more useful
 * Added NA and NaN handling for GA
 * Improved print function's styling
 * Changed GA genome len and range arguements name for more clarity
 * EvalExpression returns NA instead of NaN if expression is non-terminal
 
## 2014-05-20 - Changes in version 1.0-1

 * Public Release
