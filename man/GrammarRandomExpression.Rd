\name{GrammarGenerateRandomExpression}
\alias{GrammarGenerateRandomExpression}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Random Expression Generation
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Create random expressions from context-free grammar.
}
\usage{
GrammarRandomExpression(grammar, 
                        num.expr = 1, 
                        max.depth = length(grammar$def), 
                        startSymb = GrammarStartSymbol(grammar),
                        max.string = GetGrammarMaxSequenceRange(grammar, max.depth, startSymb), 
                        wrappings = 3, 
                        retries = 100)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{grammar}{
%%     ~~Describe \code{grammar} here~~
}
  \item{num.expr}{
%%     ~~Describe \code{num.expr} here~~
}
  \item{max.depth}{
%%     ~~Describe \code{max.depth} here~~
}
  \item{startSymb}{
%%     ~~Describe \code{startSymb} here~~
}
  \item{max.string}{
%%     ~~Describe \code{max.string} here~~
}
  \item{wrappings}{
%%     ~~Describe \code{wrappings} here~~
}
  \item{retries}{
%%     ~~Describe \code{retries} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (grammar, num.expr = 1, max.depth = length(grammar$def), 
    startSymb = grammar$startSymb, max.string = GetGrammarMaxSequenceRange(grammar, 
        max.depth, startSymb), wrappings = 3, retries = 100) 
{
    n = length(max.string)
    ret.list = list()
    for (i in 1:num.expr) {
        for (j in 1:retries) {
            genome = round(runif(n) * max.string)
            expr = GrammarGenotypeToPhenotype(genome, grammar, 
                wrappings)
            if (expr$type == "T") {
                ret.list[[length(ret.list) + 1]] = parse(text = as.character(expr$parsed))
                break
            }
        }
        if (expr$type == "NT") {
            ret.list[[length(ret.list) + 1]] = NULL
        }
    }
    if (num.expr == 1) 
        return(ret.list[[1]])
    else return(ret.list)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
