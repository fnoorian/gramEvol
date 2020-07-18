\name{GrammarRandomExpression}
\alias{GrammarRandomExpression}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Random Expression Generation
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Creates random expressions from context-free grammar.
}
\usage{
GrammarRandomExpression(grammar, 
          numExpr = 1, 
          max.depth = length(grammar$def), 
          startSymb = GrammarStartSymbol(grammar),
          max.string = GrammarMaxSequenceRange(grammar, max.depth, startSymb, 
                                               approximate = TRUE), 
          wrappings = 3, 
          retries = 100)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{grammar}{
    A \code{\link[=CreateGrammar]{grammar}} object.
  }
  \item{numExpr}{
    Number of random expressions to generate.
}
  \item{max.depth}{
    Maximum depth of recursion, in case of a cyclic grammar. 
    By default it is limited to the number of production rules in the grammar.
}
  \item{startSymb}{
    The symbol where the generation of a new expression should start.
}
  \item{max.string}{
    Maximum value for each element of the sequence.
}
  \item{wrappings}{
    The number of times the function is allowed to wrap around \code{inputString}
    if non-terminal symbols are still remaining.
}
  \item{retries}{
    Number of retries until a terminal and valid expressions is found.
}
}
\details{
\code{GrammarRandomExpression} creates \code{num.expr} random expressions from the given \code{grammar}.
It can be used to quickly examine the expressibility of the grammar, 
or as a form of random search over the grammar.
}
\value{
  An \code{expressions}, or a \code{list} of \code{expressions}.
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

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}

\examples{
# Define a simple grammar
# <expr> ::= <var><op><var>
# <op>   ::= + | - | *
# <var>  ::= A | B | C
ruleDef <- list(expr = gsrule("<var><op><var>"),
                op =   gsrule("+", "-", "*"),
                var =  grule(A, B, C))

# Create a grammar object
grammarDef <- CreateGrammar(ruleDef)

# Generate 5 random expressions
exprs <- GrammarRandomExpression(grammarDef, 5)
print(exprs)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
