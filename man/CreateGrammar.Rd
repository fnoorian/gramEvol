\name{CreateGrammar}
\alias{CreateGrammar}
\alias{print.grammar}
\alias{GetGrammarDepth}
\alias{GetGrammarMaxRuleSize}
\alias{GetGrammarMaxSequenceLen}
\alias{GetGrammarNumOfExpressions}

\title{Context-free Grammar Object}
\description{ %%  ~~ A concise (1-5 lines) description of what the function does. ~~
Create and examine a context-free grammar object. 
}
\usage{
CreateGrammar(ruleDef, startSymb = "<expr>")

GetGrammarDepth(grammar, max.depth = length(grammar$def), 
    startSymb = grammar$startSymb)

GetGrammarMaxRuleSize(grammar)

GetGrammarMaxSequenceLen(grammar, max.depth = GetGrammarDepth(grammar), 
    startSymb = grammar$startSymb)

GetGrammarNumOfExpressions(grammar, max.depth = length(grammar$def), 
    startSymb = grammar$startSymb)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ruleDef}{
    Grammatical rule definition. Either a character string or \code{\link{connection}} to a .bnf file,
    or a list of symbols and sequences 
    with syntax similar to Backus-Naur form. 
    See details. 
  }
  \item{startSymb}{
    The symbol where the generation of a new expression should start.
  }

  \item{grammar}{
    The grammar object.
  }
  \item{max.depth}{
    Maximum depth of search in case of recursive grammar.
  }
}
\details{
%%  ~~ If necessary, more details than the description above ~~
  The rule definition is the grammar described in Backus-Naur context-free grammatical format.
  \code{CreateGrammar} can read and parse .bnf files as well as a \code{list} simulating BNF format.
  Each list has to made of a \emph{symbol},
  followed by a list of production rules, i.e. possible \emph{sequences} 
  that will replace the symbol.
  If the sequence is surrounded by '<' or '>', it is considered as non-terminal
  and must be defined. Other symbols are considered terminals.
}

\value{
  \code{CreateGrammar} returns a \code{grammar} object.

  \code{GetGrammarDepth} returns depth of grammar tree.

  \code{GetGrammarMaxRuleSize} returns maximum grammar rule size.

  \code{GetGrammarMaxSequenceLen} returns maximum length of integer sequence (i.e., the chromosome)
  required for generating an expression (without resorting to wrapping).

  \code{GetGrammarNumOfExpressions} returns the number of possible expressions a 
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
ruleDef <- list(list("<expr>", list("<var><op><var>")),
                list("<op>",   list("+", "-", "*")),
                list("<var>",  list("A", "B")))

# Create a grammar object
grammarDef <- CreateGrammar(ruleDef, startSymb = "<expr>")			   

# print grammar object
print(grammarDef)

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
