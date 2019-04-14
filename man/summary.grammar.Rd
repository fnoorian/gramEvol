\name{summary}
\alias{summary.grammar}
\alias{print.summary.grammar}
\alias{GrammarStartSymbol}
\alias{GrammarGetDepth}
\alias{GrammarMaxRuleSize}
\alias{GrammarMaxSequenceLen}
\alias{GrammarNumOfExpressions}
\alias{GrammarIsRecursive}
\alias{GrammarMaxSequenceRange}
\alias{GetGrammarDepth}
\alias{GetGrammarMaxRuleSize}
\alias{GetGrammarMaxSequenceLen}
\alias{GetGrammarNumOfExpressions}

\title{Context-free Grammar Object Information}
\description{ %%  ~~ A concise (1-5 lines) description of what the function does. ~~
Examines a context-free grammar object. 
}
\usage{

\method{summary}{grammar}(object, ...)

GrammarStartSymbol(grammar)

GrammarIsRecursive(grammar, startSymb = GrammarStartSymbol(grammar), ...)

GrammarGetDepth(grammar, max.depth = max(length(grammar$def), 4), 
    startSymb = GrammarStartSymbol(grammar), ...)

GrammarMaxSequenceLen(grammar, max.depth = GetGrammarDepth(grammar), 
    startSymb = GrammarStartSymbol(grammar), ...)

GrammarMaxRuleSize(grammar)

GrammarMaxSequenceRange(grammar, max.depth = GrammarGetDepth(grammar), 
    startSymb = GrammarStartSymbol(grammar), approximate = FALSE, ...) 

GrammarNumOfExpressions(grammar, max.depth = GrammarGetDepth(grammar), 
    startSymb = GrammarStartSymbol(grammar), ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{grammar, object}{
    A \code{\link[=CreateGrammar]{grammar}} object.
  }
  \item{max.depth}{
    Maximum depth of search in case of a cyclic grammar. 
    By default it is limited to the maximum of 4 or
    the number of production rules in the grammar.
  }
  \item{startSymb}{
    The symbol where the generation of a new expression should start.
  }
  \item{approximate}{
    If True, results are approximated. Useful for recursive grammars, where number of valid expressions prohibits an accurate measurement.
  }
  \item{...}{
    unused inputs.
  }
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%  A grammar object can be examined through several functions.
%  \code{summary} function can be used to get them all.
%}

\value{

\code{summary} returns a \code{summary.grammar} object, with the following slots which
are obtained from the other functions:
  \item{Start.Symbol}{\code{GrammarStartSymbol} returns the grammar's starting symbol.}
  \item{Is.Recursive }{\code{GrammarIsRecursive} returns TRUE if grammar contains a recursive element.}
  \item{Tree.Depth}{\code{GrammarGetDepth} returns the depth of the grammar.
  It is limited to \code{max.depth} for a recursive grammar.}
  \item{Maximum.Sequence.Length}{\code{GrammarMaxSequenceLen} returns the maximum length of a sequence needed to
 generate an expression without wrapping.}
  \item{Maximum.Rule.Size}{\code{GrammarMaxRuleSize} returns the largest rule size in the grammar.}
  \item{Maximum.Sequence.Variation}{\code{GrammarMaxSequenceRange} returns a numeric sequence, with each of its elements
holding the highest range that the same position in all sequences can hold.}
  \item{No.of.Unique.Expressions}{\code{GrammarNumOfExpressions} returns the number of expressions a grammar can generate.}
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
\code{\link{GrammarMap}}
}
\examples{
# Define a simple grammar
# <expr> ::= <var><op><var>
# <op>   ::= + | - | *
# <var>  ::= A | B
ruleDef <- list(expr = gsrule("<var><op><var>"),
                op   = gsrule("+", "-", "*"),
                var  = gsrule("A", "B"))

# Create a grammar object
grammarDef <- CreateGrammar(ruleDef)			   

# summarize grammar object
summary(grammarDef)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
