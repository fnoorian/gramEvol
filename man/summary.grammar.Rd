\name{summary.grammar}
\alias{summary.grammar}
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

\title{Context-free Grammar Object}
\description{ %%  ~~ A concise (1-5 lines) description of what the function does. ~~
Examine a context-free grammar object. 
}
\usage{

\method{summary}{grammar}(object, ...)

GrammarStartSymbol(grammar)

GrammarGetDepth(grammar, max.depth = length(grammar$def), 
    startSymb = GrammarStartSymbol(grammar))

GrammarMaxRuleSize(grammar)

GrammarMaxSequenceLen(grammar, max.depth = GetGrammarDepth(grammar), 
    startSymb = GrammarStartSymbol(grammar))

GrammarNumOfExpressions(grammar, max.depth = length(grammar$def), 
    startSymb = GrammarStartSymbol(grammar))

GrammarMaxSequenceRange(grammar, max.depth = length(grammar$def), 
  startSymb = GrammarStartSymbol(grammar)) 

GrammarIsRecursive(grammar, startSymb = GrammarStartSymbol(grammar))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{grammar, object}{
    The grammar object as created by \code{\link{CreateGrammar}}.
  }
  \item{max.depth}{
    Maximum depth of search in case of recursive grammar. 
    By default it is limited to the number of production rules in the grammar.
  }
  \item{startSymb}{
    The symbol where the generation of a new expression should start.
  }
  \item{...}{
    unused inputs.
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

# print grammar object
print(grammarDef)

# summarize grammar object
summary(grammarDef)

# print information about the grammar
print(paste("Depth:", GetGrammarDepth(grammarDef))) 
print(paste("Max Rule Size:", GetGrammarMaxRuleSize(grammarDef)))
print(paste("Max Sequence Len:", GetGrammarMaxSequenceLen(grammarDef)))
print(paste("Num Of Expressions:", GetGrammarNumOfExpressions(grammarDef))) 
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
