\name{GrammarGenotypeToPhenotype}
\alias{GrammarGenotypeToPhenotype}
\title{
Convert an integer sequence to an expression
}
\description{
Using a grammar created by \code{\link{CreateGrammar}},
converts a sequence of integer numbers to an expression.
}
\usage{
GrammarGenotypeToPhenotype(inputString, grammar, wrappings = 3)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{inputString}{
A vector of integers to define the path of symbol selection in grammar tree. It uses
zero-based indexing to address production rules in the grammar.
}
  \item{grammar}{
The grammar object as created by \code{\link{CreateGrammar}}.
}
  \item{wrappings}{
The number of times the function is allowed to wrap around \code{inputString}
if non-terminal symbols are still remaining.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
\code{GrammarGenotypeToPhenotype} starts from the \code{startExpr} given in the
definition of grammar in \code{\link{CreateGrammar}}. ;
then it iterates through \code{inputString}, replacing symbols in the expression
with associated replacements in the grammar using the current value of
\code{inputString}.
If the function exhausts all non-terminal symbols in the expression, it terminates.
If the end of \code{inputString} is reached and  still non-terminal symbols
exist, the algorithm will start from the beginning of the \code{inputString}
again. To avoid unlimited run by recursions in case of a cyclic grammar,
\code{wrappings} variable limits the number of this restart.
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
  \item{expr}{The generated expression as a character string.}
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
\code{\link{CreateGrammar}},
\code{\link{GrammaticalEvolution}}
}

\examples{
# Define a simple grammar
# <expr> := <var><op><var>
# <op> := "+" | "-" | "*"
# <var> := "A" | "B"
ruleDef <- list(list("expr", list("<var><op><var>")),
                list("op",   list("+", "-", "*")),
                list("var",  list("A", "B", "C")))
			   
# Create a grammar object
grammarDef <- CreateGrammar(ruleDef, startSymb = "<expr>")			   
print(grammarDef)

# this should create the expression "A - C"
# <expr>         -> 0 -> <var><op><var>
# <var><op><var> -> 0 -> A<op><var>
# A<op><var>     -> 1 -> A - <var>
# A - <var>      -> 2 -> A - C
sq <- c(0, 0, 1, 2)
expr <- GrammarGenotypeToPhenotype(sq, grammarDef)

print(expr)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
