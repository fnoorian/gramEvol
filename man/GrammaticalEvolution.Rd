\name{GrammaticalEvolution}
\alias{GrammaticalEvolution}

\title{Grammatical Evolution}
\description{
Evolves an expression using a context-free grammar
to minimize a given fitness function.
}
\usage{
GrammaticalEvolution(grammarDef, fitnessFunction, 
    seqLen = GetGrammarMaxSequenceLen(grammarDef), 
    wrappings = 3, elitism = 2, popSize = 50, iterations = 100, 
    terminationFitness = NA, mutationChance = NA, numExpr = 1,
    suggestions = NULL, monitorFunc=NULL, plapply = lapply, ...)
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
  \item{seqLen}{
    Length of integer vector used to create the expression.
}
 \item{wrappings}{
    Number of wrappings in case the length of chromosome is not enough for conversion to an expression.
}
  \item{elitism}{
    Number of elite individuals in genetic algorithm.
}
  \item{popSize}{
    Population size in genetic algorithm.
}
  \item{iterations}{
    Number of maximum iterations in genetic algorithm.
}
  \item{terminationFitness}{
    Target fitness in genetic algorithm.
}
  \item{mutationChance}{
    Mutation chance in genetic algorithm. It must be between 0 and 1.
    By default it is set to \code{1/(1+chromosome size))}.
}
  \item{numExpr}{
    Number of expressions generated and given to \code{fitnessFunction}.
}
  \item{suggestions}{
    List of suggested chromosomes to be added to initial population pool.
}
 \item{monitorFunc}{
  A function that is called at each generation. It can be used to monitor evolution of population.
}
  \item{plapply}{
    \code{lapply} function used for mapping chromosomes to fitness function.
    See details for parallelization tips.
}
  \item{\dots}{
    Additional parameters are passed to \code{\link{GeneticAlg.int}}.
}
}
\details{
  This function offers a tiny wrapper around \code{\link{GeneticAlg.int}}.
  It evolves integer sequences and converts them to a collection containing
  \code{numExpr} strings, which are representations of
  analytical expressions. These expressions can be evaluated using \code{eval} function.
  The \code{fitnessFunction} receives these strings and must return a numeric value.
  The goal of optimization would be to find a chromosome which minimizes this value.

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
  \item{bestExpression}{String representation of expression(s) with the best fitness.}
  \item{gaSummary}{
      The results of \code{\link{GeneticAlg.int}}.}
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
ruleDef <- list(list("<expr>",     list("<der-expr><op><der-expr>")),
                list("<der-expr>", list("<func>(<var>)", "<var>")),
                list("<func>",     list("log", "exp", "sin", "cos")),
                list("<op>",       list("+", "-", "*")),
                list("<var>",      list("A", "B", "<n>")),
                list("<n>",        list("1", "2", "3", "4")))

# Creating the grammar object
grammarDef <- CreateGrammar(ruleDef, startSymb = "<expr>")

# fitness function
fitnessFunction <- function(expr) {
# expr: a string containing a symbolic expression
# returns: Symbolic regression Error
  A <- 1:6
  B <- c(2, 5, 8, 3, 4, 1)

  result <- EvalExpressions(expr)

  X <- log(A) * B
  err <- sum((result - X)^2)
  
  return(err)
}

# invoke grammatical evolution (with default parameters)
ge <- GrammaticalEvolution(grammarDef, fitnessFunction)

# print results
print(ge$bestExpression)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
