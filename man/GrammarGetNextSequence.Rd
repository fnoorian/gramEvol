\name{GrammarGetNextSequence}

\alias{GrammarGetNextSequence}
\alias{GrammarGetFirstSequence}
\alias{is.GrammarOverflow}
\alias{print.GrammarOverflow}

\title{Grammar Iterator}
\description{ %%  ~~ A concise (1-5 lines) description of what the function does. ~~
Iterate through grammar's valid sequences.
}
\usage{

GrammarGetFirstSequence(grammar, 
            seqStart = NULL, 
            startSymb = GrammarStartSymbol(grammar), 
            max.len = GrammarMaxSequenceLen(grammar))

GrammarGetNextSequence(grammar, 
            seqStart = NULL, 
            startSymb = GrammarStartSymbol(grammar), 
            max.len = GrammarMaxSequenceLen(grammar))
  
is.GrammarOverflow(object)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{grammar}{
    The grammar object as created by \code{\link{CreateGrammar}}.
  }
  \item{seqStart}{
    The sequence to be incremented. If \code{NULL}, the first sequence is automatically assumed.
    Maximum depth of search in case of recursive grammar. 
    Partial Sequences can be entered.
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


print(grammar)

summary(grammar)

GrammarStartSymbol(grammar)

GrammarGetDepth(grammar, max.depth = length(grammar$def), 
    startSymb = grammar$startSymb)

GrammarMaxRuleSize(grammar)

GrammarMaxSequenceLen(grammar, max.depth = GetGrammarDepth(grammar), 
    startSymb = grammar$startSymb)

GrammarNumOfExpressions(grammar, max.depth = length(grammar$def), 
    startSymb = GrammarStartSymbol(grammar))

GrammarIsRecursive(grammar, startSymb = GrammarStartSymbol(grammar), 
    symb.list = NULL)
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
\code{\link{GrammarGenotypeToPhenotype}},
\code{\link{GrammaticalEvolution}}
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
