\name{c}
\alias{c.GERule}

\title{
  Grammar Rule Concatenation
}
\description{
Concatenates two or more grammar rule objects.
}
\usage{
  \method{c}{GERule}(..., recursive=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{...}{
    Grammar rule objects to be concatenated.
  }
  \item{recursive}{
  Not used.
  }
}

\value{
A new grammar rule object.
}

\seealso{
\code{\link{CreateGrammar}}
}
\examples{
rule1 <- grule(Func1, Func2)
rule2 <- grule(`*`, `/`)

rule.all <- c(rule1, rule2)

print(rule.all)
}
