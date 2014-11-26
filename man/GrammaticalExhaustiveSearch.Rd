\name{GrammaticalExhaustiveSearch}

\alias{GrammaticalExhaustiveSearch}


\title{Exhaustive Search}
\description{ %%  ~~ A concise (1-5 lines) description of what the function does. ~~
Exhaustive Search with in context-free grammar.
}
\usage{

GrammaticalExhaustiveSearch(grammar, evalFunc,
                           max.depth = length(grammar$def), 
                           startSymb = GrammarStartSymbol(startSymb),
                           wrappings = 3,
                           terminationFitness = NA,
                           monitor = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{grammar}{
    The grammar object as created by \code{\link{CreateGrammar}}.
  }
 \item{evalFunc}{
    The evaluation function, taking an expression its input
    and returning the fitness of the expression.
 }
  \item{object}{
    Any Object.
  }
}
\details{
%%  ~~ If necessary, more details than the description above ~~
  A grammar object, created using\code{\link{CreateGrammar}} can be examined through several
  functions.

  The rule definition is the grammar described in Backus-Naur context-free grammatical format.
  \code{CreateGrammar} can read and parse .bnf files as well as a \code{list} simulating BNF format.
  Each list has to made of a \emph{symbol},
  followed by a list of production rules, i.e. possible \emph{sequences} 
  that will replace the symbol.
  If the sequence is surrounded by '<' or '>', it is considered as non-terminal
  and must be defined. Other symbols are considered terminals.

}

\value{
  \code{CreateGrammar} returns a \code{grammar} object.

  \code{GetGrammarDepth} returns depth of grammar tree.

  \code{GetGrammarMaxRuleSize} returns maximum grammar rule size.

  \code{GrammarMaxSequenceLen} returns maximum length of integer sequence (i.e., the chromosome)
  required for generating an expression (without resorting to wrapping).

  \code{GrammarNumOfExpressions} returns the number of possible expressions a 
  grammar can generate.

}

%\references{
%% ~put references to the literature/web site here ~
%}
%\author{
%%  ~~who you are~~
%}
%\note{
%%  ~~further notes~~
%}
%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{GrammarGetNextSequence}},
\code{\link{GrammaticalEvolution}}
}
\examples{
library("gramEvol")

ruleDef <- list(expr = gsrule("<var><op><var>"),
                op   = gsrule("+", "-", "*"),
                var  = gsrule("A", "B"))

# Create a grammar object
grammarDef <- CreateGrammar(ruleDef)         


# use exhaustive search to find the sequence for creating "B - A"
fitness <- function(expr) {
  if (as.character(expr) == "B - A") {
    return(0) # Minimum error
  } else {
    return(1) # maximum error
  }
}

res = GrammaticalExhaustiveSearch(grammarDef, fitness)

print(res$bestSequence)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
