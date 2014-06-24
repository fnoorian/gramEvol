\name{EvalExpressions}
\alias{EvalExpressions}

\title{
 Evaluate a collection of Expressions
}
\description{
\code{EvalExpressions} evaluates expressions, either in string format
or as a grammar generated expression.
}
\usage{
EvalExpressions(expressions, envir = parent.frame())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{expressions}{
  an expression, or a collection of expressions. It should be either in character
    string format or be the output of \link{GrammarGenotypeToPhenotype}.
}
  \item{envir}{
   the \code{environment} in which expressions are to be evaluated.  May
          also be \code{NULL}, a list, a data frame, a pair-list or an
          integer as specified in \code{sys.call}.
	}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
\code{EvalExpressions} is a wrapper around 
\code{eval} and \code{parse} functions in R base package.
It can handle a single, a collection or a list of expressions
in string format or as the output of \code{\link{GrammarGenotypeToPhenotype}}.

The \code{envir} argument is directly passed to \code{eval} function. If it is not specified,
the parent frame (i.e., the environment where the call to \code{eval} was made) is used instead.	

\code{EvalExpressions} only evaluates terminal expressions and character strings.
Evaluating non-terminal expressions will result in a warning and return of NA.
}
\value{
%%  ~Describe the value returned
If one expression is evaluated, a vector of numeric values is returned.
Otherwise a data frame with result of each expression in a separate column is returned.
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
\code{\link{GrammarGenotypeToPhenotype}},
\code{\link{GrammaticalEvolution}}
}

\examples{

A <- 1:6
B <- 1

EvalExpressions("A - B")

exprs <- c("A + B", "A - B")
EvalExpressions(exprs, data.frame(A = A, B = B))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
