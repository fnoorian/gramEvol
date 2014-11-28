\name{GrammaticalEvolution}
\alias{GrammaticalEvolution}
\alias{print.GrammaticalEvolution}

\title{Grammatical Evolution}
\description{
Evolves an expression using a context-free grammar
to minimize a given fitness function.
}
\usage{
GrammaticalEvolution(grammarDef, fitnessFunction, 
                                  numExpr = 1, 
                                  max.depth = GrammarGetDepth(grammarDef),
                                  startSymb = GrammarStartSymbol(grammarDef),
                                  seqLen = GrammarMaxSequenceLen(grammarDef, max.depth, startSymb),
                                  wrappings=3, 
                                  suggestions=NULL,
                                  optimizer = c("es", "ga"),
                                  popSize=8, newPerGen = "auto", elitism = 2,
                                  mutationChance=NA,
                                  iterations=1000, terminationFitness=NA, 
                                  monitorFunc=NULL,
                                  plapply=lapply, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{grammarDef}{
    \code{grammar} object, created by \code{\link{CreateGrammar}}.
}
  \item{fitnessFunction}{
    The fitness function, taking a string or a collection of strings
    containing the expression(s) as its input
    and returning the fitness of the expression(s).
}
  \item{numExpr}{
    Number of expressions generated and given to \code{fitnessFunction}.
}
 \item{max.depth}{
    Maximum depth of search in case of recursive grammar. 
    By default it is limited to the number of production rules in the grammar.
 }
  \item{startSymb}{
    The symbol where the generation of a new expression should start.
}
  \item{seqLen}{
    Length of integer vector used to create the expression.
}
 \item{wrappings}{
    Number of wrappings in case the length of chromosome is not enough for conversion to an expression.
}
  \item{suggestions}{
    List of suggested chromosomes to be added to initial population pool.
}
  \item{optimizer}{
    The evolutionary optimizer. "es" uses evolution strategy as in \code{\link{EvolutionStrategy.int}}
    and "ga" uses genetic algorithm as in \code{\link{GeneticAlg.int}}.
}
  \item{popSize}{
    Population size in the evolutionary optimizer.
}
  \item{newPerGen}{
    Number of randomly generated individuals in evolution strategy.
    If "auto", it is set to 25% of population of grammar if it is not recursive,
    otherwise to all of it.
}
  \item{elitism}{
  Number of top ranking chromosomes that are directly transfered to next generation without
  going through evolutionary operations, used in genetic algorithm optimizer.
}

  \item{iterations}{
    Number of maximum iterations in the evolutionary optimizer.
}
  \item{terminationFitness}{
    Target fitness in the evolutionary optimizer.
}
  \item{mutationChance}{
    Mutation chance in the evolutionary optimizer. It must be between 0 and 1.
    By default it is set to \code{1/(1+chromosome size))} for genetic algorithm and
    \code{10/(1+chromosome size))} for evolutionary algorithm.
}
 \item{monitorFunc}{
  A function that is called at each generation. It can be used to monitor evolution of population.
}
  \item{plapply}{
    \code{lapply} function used for mapping chromosomes to fitness function.
    See details for parallelization tips.
}
  \item{\dots}{
    Additional parameters are passed to \code{\link{GeneticAlg.int}} or \code{\link{EvolutionStrategy.int}}.
}
}
\details{
  This function an evolutionary search over the grammar, known as Grammatical Evolution.
  It evolves integer sequences and converts them to a collection containing
  \code{numExpr} strings, which are representations of
  analytical expressions. These expressions can be evaluated using \code{eval} function.
  The \code{fitnessFunction} receives these strings and must return a numeric value.
  The goal of optimization would be to find a chromosome which minimizes this value.

  Two evolutionary optimizers are supported: Genetic algorithm and evolution strategy
  which are set by the \code{optimizer} parameter.

  Only valid (i.e., terminal) expressions are passed to \code{fitnessFunction},
  and it is guaranteed that \code{fitnessFunction} receives at least one string. 

  If the grammar contains recursive elements, it is advisable that \code{chromosomeLen} is 
  defined manually, as in such cases the possible search space grows explosively
  with the recursion. The evolutionary algorithm automatically removes
  the recursive chromosomes from the population by imposing a penalty for
  chromosomes creating expressions with non-terminal elements.

  \code{monitorFunc} receives a list similar to the \code{GrammaticalEvolution}'s return value.
}
\value{
  The results of \code{\link{GeneticAlg.int}} or \code{\link{EvolutionStrategy.int}} with an additional item:
  \item{best$expressions}{Expression(s) with the best fitness.}
}

% \references{
% %% ~put references to the literature/web site here ~
% }
% \author{
% %%  ~~who you are~~
% }
% \note{
% %%  ~~further notes~~
% }
% %% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  \code{\link{CreateGrammar}}, 
  \code{\link{GeneticAlg.int}},
  \code{\link{EvalExpressions}}
}
\examples{
# Grammar Definition
ruleDef <- list(expr     = gsrule("<der.expr><op><der.expr>"),
                der.expr = grule(func(var), var),
                func     = grule(log, exp, sin, cos),
                op       = gsrule("+", "-", "*"),
                var      = grule(A, B, n),
                n        = grule(1, 2, 3, 4))

# Creating the grammar object
grammarDef <- CreateGrammar(ruleDef)

# fitness function
fitnessFunction <- function(expr) {
  # expr: a string containing a symbolic expression
  # returns: Symbolic regression Error
  A <- 1:6
  B <- c(2, 5, 8, 3, 4, 1)
  
  result <- eval(as.expression(expr))
  
  X <- log(A) * B
  err <- sum((result - X)^2)
  
  return(err)
}

# invoke grammatical evolution (with default parameters)
ge <- GrammaticalEvolution(grammarDef, fitnessFunction)

# print results
print(ge$best$expressions)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
