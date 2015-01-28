\name{GrammarIsTerminal}
\alias{GrammarIsTerminal}

\title{
Non-terminal Phenotype test.
}
\description{
Checks a phenotype object for containing non-terminal symbols.
}
\usage{
GrammarIsTerminal(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
  		A \code{\link[=GrammarMap]{GEPhenotype}} object.
	}
}
%\details{
%}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
  \code{TRUE} if phenotype is terminal, \code{FALSE} otherwise.
%% ...
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
\code{\link{GrammarMap}}
}

\examples{
# Define a recursive grammar
# <expr> ::= <expr>+<expr> | var
# <var>  ::= A | B | C
ruleDef <- list(expr = grule(expr+expr, var),
                var =  grule(A, B, C))

# Create a grammar object
grammarDef <- CreateGrammar(ruleDef)

# a short sequence leading to infinite recursion
sq <- c(0)
expr <- GrammarMap(sq, grammarDef)

print(expr)

# check the phenotype for being non-terminal
print(GrammarIsTerminal(expr))


# a terminal sequence
sq <- c(0, 1, 0, 1, 2)
expr <- GrammarMap(sq, grammarDef)

print(expr)
print(GrammarIsTerminal(expr))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
