\name{GrammarGetNextSequence}

\alias{GrammarGetNextSequence}
\alias{GrammarGetFirstSequence}
\alias{is.GrammarOverflow}
\alias{print.GrammarOverflow}

\title{Grammar Iterator}
\description{ %%  ~~ A concise (1-5 lines) description of what the function does. ~~
Iterates through grammar's valid sequences.
}
\usage{

GrammarGetFirstSequence(grammar, 
            seqStart = NULL, 
            startSymb = GrammarStartSymbol(grammar),
            max.depth = GrammarGetDepth(grammar),
            max.len = GrammarMaxSequenceLen(grammar, max.depth, startSymb))

GrammarGetNextSequence(grammar, 
            seqStart = NULL, 
            startSymb = GrammarStartSymbol(grammar),
            max.depth = GrammarGetDepth(grammar),
            max.len = GrammarMaxSequenceLen(grammar, max.depth, startSymb))
  
is.GrammarOverflow(object)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{grammar}{
    A \code{\link[=CreateGrammar]{grammar}} object.
  }
  \item{seqStart}{
    The sequence to be incremented. 
    For a value of \code{NULL}, the first sequence is returned.
    Partial sequences are completed and returned.
  }
  \item{startSymb}{
    The non-terminal symbol where the generation of a new expression should start.
  }
  \item{max.depth}{
    Maximum depth of recursion, in case of a cyclic grammar.
    By default it is limited to the number of production rules in the grammar.
  }
  \item{max.len}{
    Maximum length of sequence to return. Used to avoid recursion.
  }
  \item{object}{
    An object to be tested.
  }
}
\details{
%%  ~~ If necessary, more details than the description above ~~
  \code{GrammarGetFirstSequence} returns the first sequence that creates a valid expression
  with the given \code{\link[=CreateGrammar]{grammar}} object.
  \code{GrammarGetNextSequence} allows iterating through all valid sequences in a grammar.
  If a \code{seqStart = NULL} is used, \code{GrammarGetFirstSequence} is called to 
  and the first sequence in the grammar is returned.
  Calling \code{GrammarGetNextSequence} or \code{GrammarGetFirstSequence} with an incomplete
  sequence returns a full-length sequence starting with the given \code{seqStart}.

  When \code{GrammarGetNextSequence} reaches the last of all valid sequences, it returns
  a \code{GrammarOverflow} object. This object can be identified using \code{is.GrammarOverflow}.
}

\value{
  \code{GrammarGetFirstSequence} returns a numeric vector representing the first sequence of the grammar.

  \code{GrammarGetNextSequence} returns a numeric vector or a \code{GrammarOverflow} object.

  \code{is.GrammarOverflow} returns TRUE if \code{object} is a \code{GrammarOverflow}, otherwise FALSE.
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
\code{\link{GrammaticalExhaustiveSearch}}
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

# Iterate and print all valid sequence and expressions
string <- NULL
while (TRUE) {
  string <- GrammarGetNextSequence(grammarDef, string)

  if (is.GrammarOverflow(string)) {
    break
  }

  expr <- GrammarMap(string, grammarDef)
  cat(string, " -> ", as.character(expr), "\n")
}

# test a partial string
GrammarGetNextSequence(grammarDef, c(0, 0, 2))

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
