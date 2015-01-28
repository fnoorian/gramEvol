\name{GrammarMap}
\alias{GrammarMap}
\alias{GrammarGenotypeToPhenotype}
\alias{as.character.GEPhenotype}
\alias{as.expression.GEPhenotype}
\alias{print.GEPhenotype}

\title{
Sequence to Expression Mapping using Context-free Grammar
}
\description{
Converts a sequence of integer numbers to an expression
using a \code{grammar} object.
}
\usage{
GrammarMap(inputString, grammar, wrappings = 3, verbose = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{inputString}{
		A vector of integers to define the path of symbol selection in grammar tree. It uses
		zero-based indexing to address production rules in the grammar.
	}
  
  \item{grammar}{
    A \code{\link[=CreateGrammar]{grammar}} object.
	}
  
  \item{wrappings}{
		The number of times the function is allowed to wrap around \code{inputString}
		if non-terminal symbols are still remaining.
	}
  \item{verbose}{
		Prints out each steps of grammar mapping.
	}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
\code{GrammarMap} starts from the \code{startExpr} defined in the 
\code{\link[=CreateGrammar]{grammar}} object;
then it iterates through \code{inputString}, replacing symbols in the expression
with associated replacements in the grammar using the current value of
\code{inputString}.

If the function exhausts all non-terminal symbols in the expression, it terminates.
If the end of \code{inputString} is reached and  still non-terminal symbols
exist, the algorithm will restart from the beginning of the current \code{inputString}. 
To avoid unlimited recursions in case of a cyclic grammar,
\code{wrappings} variable limits the number of this restart.

If \code{verbose = TRUE}, step-by-step replacement of symbols with production rules are displayed.

\code{GrammarMap} returns a \code{GEPhenotype} object, which can be converted to
a character string using \code{as.character}, or an R expression with \code{as.expression}.
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
  A \code{GrammarMap} returns a \code{GEPhenotype} object.
  \item{expr}{The generated expression as a character string.}
  \item{parsed}{The generated expression. NULL if the expression still 
				contains non-terminal symbols.}
  \item{type}{"T" if the expression is valid, "NT" if the expression still 
				contains non-terminal symbols. }
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
\code{\link{GrammarIsTerminal}}
\code{\link{CreateGrammar}},
\code{\link{GrammarRandomExpression}}
}

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

# this should create the expression "A - C"
# <expr>         -> 0 -> <var><op><var>
# <var><op><var> -> 0 -> A<op><var>
# A<op><var>     -> 1 -> A - <var>
# A - <var>      -> 2 -> A - C
sq <- c(0, 0, 1, 2)
expr <- GrammarMap(sq, grammarDef, verbose = TRUE)

print(expr)

# check the expression as a character string
stopifnot(as.character(expr) == "A - C")

# evaluate the expression
A = 5; C = 1
eval(as.expression(expr))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
