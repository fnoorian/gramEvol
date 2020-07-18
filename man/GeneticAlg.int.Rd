\name{GeneticAlg.int}
\alias{GeneticAlg.int}
\alias{print.GeneticAlg.int}

\title{ 
  Genetic Algorithm with Integer Chromosomes
}
\description{
Uses genetic algorithm to find the minima of a given cost function.
It evolves chromosomes with limited-range integers as codons.
}
\usage{
GeneticAlg.int(genomeLen, codonMin, codonMax, 
    genomeMin = rep.int(codonMin, genomeLen), 
    genomeMax = rep.int(codonMax, genomeLen), 
    suggestions = NULL, popSize = 50, 
    iterations = 100, terminationCost = NA, 
    mutationChance = 1/(genomeLen+1), elitism = floor(popSize/10), 
    geneCrossoverPoints = NULL,
    monitorFunc = NULL, evalFunc, allowrepeat = TRUE, 
    showSettings = FALSE, verbose = FALSE, plapply = lapply)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{genomeLen}{
  Number of integers (i.e, codons) in chromosome.
}
  \item{codonMin}{
  Minimum integer value range for all codons.
}
  \item{codonMax}{
  Maximum integer value range for all codons.
}
  \item{genomeMin}{
  A vector of length \code{genomeLen} containing fine-grained control
  over each codon's minimum. Overrides \code{codonMin}.
}
  \item{genomeMax}{
  A vector of length \code{genomeLen} containing fine-grained control
  over each codon's maximum. Overrides \code{codonMax}.
}
  \item{suggestions}{
  A list of suggested chromosomes to be used in the initial population.
  Alternatively, an m-by-n matrix, where m is the number of suggestions and n is the chromosome length.
}
  \item{popSize}{
  Size of the population.
}
  \item{iterations}{
  Number of generations to evolve the population.
}
  \item{terminationCost}{
  Target cost. If the best chromosome's cost reaches this value
   the algorithm terminates.
}
  \item{mutationChance}{
  The chance of a codon being mutated. It must be between 0 and 1.
}
  \item{geneCrossoverPoints}{
  Codon groupings (genes) to be considered while crossover occurs. 
  If given, odd and even codon groups are exchanged between parents.
  Otherwise random points are selected and a classic single-point crossover is performed.
}
  \item{elitism}{
  Number of top ranking chromosomes that are directly transfered to next generation without
  going through evolutionary operations.
}
  \item{monitorFunc}{
  A function that is called at each generation. Can be used to monitor evolution of population.
}
  \item{evalFunc}{
  The cost function.
}
  \item{allowrepeat}{
  Allows or forbids repeated integers in the chromosome.
}
  \item{showSettings}{
  Enables printing GA settings.
}
  \item{verbose}{
  Enables verbose debugging info.
}
  \item{plapply}{
  \code{lapply} function used for mapping chromosomes to cost function.
  See details for parallelization tips.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
\code{GeneticAlg.int} implements evolutionary algorithms with
chromosomes created from integer values in the range of \code{codonMin} to
\code{codonMax}. \code{genomeMin} and \code{genomeMax} allow fine-grained
control of range for individual codons.
It first creates an initial population, using suggested inputs
\code{suggestions} and randomly generated chromosomes. 
Cost of each chromosome is evaluated using the cost function
\code{evalFunc}. If one of the chromosomes reaches
\code{terminationCost}, the algorithm terminates; 
Otherwise evolutionary operators including selection, cross-over
and mutation are applied to the population to create a new generation.
This iteration is continued until the required cost is reached
or the number of generations exceeds \code{iterations}.

At each generation, the supplied \code{monitorFunc} is called with a
list similar to \code{GeneticAlg.int} returning value as its argument. 

The \code{evalFunc} receives integer sequences and must return a numeric value.
The goal of optimization would be to find a chromosome which minimizes this value.

To parallelize cost function evaluation, set \code{plapply} to a parallelized
\code{lapply}, such as \code{mclapply} from package \code{parallel}. 
In functions that do not handle data dependencies such as \code{parLapply}, 
variables and functions required for correct execution of \code{evalFunc} 
must be exported to worker nodes before invoking \code{GeneticAlg.int}.
}
\value{

  A list containing information about 
  \code{settings}, \code{population}, and the \code{best} chromosome.

  \item{settings$genomeMin}{
    Minimum of each codon. }

  \item{Settings$genomeMax}{
    Maximum of each codon. }

  \item{settings$popSize}{
    Size of the population.}

  \item{settings$elitism}{
    Number of elite individuals.}

  \item{settings$iterations}{
    Number of maximum generations.}

  \item{settings$suggestions}{
    Suggested chromosomes.}

  \item{settings$mutationChance}{
    Mutation chance.}

  \item{settings$geneCrossoverPoints}{
    Cross-over points.}

  \item{population$population}{
    The genomic data of the current population.}

  \item{population$evaluations}{
    Cost of the latest generation.}

  \item{population$best}{
    Historical cost of the best chromosomes.}

  \item{population$mean}{
    Historical mean cost of population.}

  \item{population$currentIteration}{
   Number of generations evolved until now.}
     
  \item{best$genome}{
    The best chromosome.}

  \item{best$cost}{
    The cost of the best chromosome.}
}

\references{
This function is partially inspired by \code{genalg} package
by Egon Willighagen. See \url{https://cran.r-project.org/package=genalg}.
}

% \author{
% %%  ~~who you are~~
% }
% \note{
% %%  ~~further notes~~
% }
%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{GrammaticalEvolution}}, 
\code{\link{EvolutionStrategy.int}}
}

\examples{

# define the evaluate function
evalfunc <- function(l) {
    # maximize the odd indices and minimize the even indices
    # no repeated values are allowed
    odd <- seq(1, 20, 2)
    even <- seq(2, 20, 2)
    err <- sum(l[even]) - sum(l[odd]);

    stopifnot(!any(duplicated(l))) # no duplication allowed

    return (err)
}

monitorFunc <- function(result) {
    cat("Best of gen: ", min(result$best$cost), "\n")
}

x <- GeneticAlg.int(genomeLen = 20, codonMin = 0, codonMax = 20,
                allowrepeat = FALSE, terminationCost = -110,
                monitorFunc = monitorFunc, evalFunc = evalfunc)

print(x)

best.result <- x$best$genome
print("Odds:")
print(sort(best.result[seq(1, 20, 2)]))
print("Evens:")
print(sort(best.result[seq(2, 20, 2)]))
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
