\name{CreateGrammar}
\alias{CreateGrammar}
\alias{grule}
\alias{gsrule}
\alias{print.grammar}
\alias{print.GERule}
\alias{c.GERule}

\title{Context-free Grammar Object}
\description{ %%  ~~ A concise (1-5 lines) description of what the function does. ~~
Creates a context-free grammar object. 
}
\usage{
  grule(...)

  gsrule(...)

  CreateGrammar(ruleDef, startSymb)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{...}{
    A series of comma separated strings or expressions, for \code{gsrule} and
    \code{grule} respectively. Expressions can be wrapped in \code{.()} to preserve their 
    commas or assignment operators.
  }
  \item{ruleDef}{
    Grammatical rule definition. Either a list of grammar rule objects (\code{GERule}) created using \code{grule} and \code{gsrule}
    with a syntax similar to Backus-Naur form,
    or a list of character strings representing symbols and sequences in Backus-Naur form,
    or a filename or \code{\link{connection}} to a .bnf file. 

    See details. 
  }
  \item{startSymb}{
    The symbol where the generation of a new expression should start.
    If not given, the first rule in \code{ruleDef} is used.
  }
}
\details{
%%  ~~ If necessary, more details than the description above ~~
  The rule definition is the grammar described in Backus-Naur context-free grammatical format.
  The preferred way of defining a grammar is to create a \code{list} simulating BNF format,
  which collects several named grammar rule objects (\code{GERule}).
  Each name defines the \emph{non-terminal symbol}, and each rule 
  in the collection determines the \emph{production rule}, 
  i.e., possible \emph{sequences} that will replace the symbol.

  Defining a grammar rule object (\code{GERule}) can take two forms:

  1. The first form uses \code{grule} (Grammar Rule), where R expressions are accepted. In the mapping process,
   variables are looked up and replaced using the production rules.

  2. The second form uses \code{gsrule} (Grammar String Rule) and uses character strings. The input to \code{gsrule} are character string values, where any value surrounded by '<' or '>' is considered as \emph{non-terminal symbols} and
  will be replaced using the rule with the same name in the mapping process. Other symbols are considered terminals. This form allows generation of sequences that are not syntactically valid in R (such as \code{`var op var`}).

  Alternatively, \code{CreateGrammar} can read and parse .bnf text files.
}

\value{
  \code{CreateGrammar} returns a \code{grammar} object.

  \code{grule} and \code{gsrule} return a \code{GERule} object.
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
\code{\link{GrammarMap}},
\code{\link{GrammaticalEvolution}}
}
\examples{

# Define a simple grammar in BNF format
# <expr> ::= <var><op><var>
# <op>   ::= + | - | *
# <var>  ::= A | B
ruleDef <- list(expr = gsrule("<var><op><var>"),
                op   = gsrule("+", "-", "*"),
                var  = gsrule("A", "B"))

# print rules
print(ruleDef)

# Create a grammar object
grammarDef <- CreateGrammar(ruleDef)

# print grammar object
print(grammarDef)

# Creating the same grammar using R expressions
ruleDef <- list(expr = grule(op(var, var)),
                op   = grule(`+`, `-`, `*`),
                var  = grule(A, B))

grammarDef <- CreateGrammar(ruleDef)

print(grammarDef)

# Two rules with commas and assignments, preserved using .()
ruleDef <- list(expr = grule(data.frame(dat)),
                dat  = grule(.(x = 1, y = 2), .(x = 5, y = 6)))
grammarDef <- CreateGrammar(ruleDef)
print(GrammarMap(c(0), grammarDef))
print(GrammarMap(c(1), grammarDef))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
