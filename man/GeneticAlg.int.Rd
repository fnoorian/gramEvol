\name{GeneticAlg.int}
\alias{GeneticAlg.int}
\title{ 
  Genetic Algorithm with integer chromosomes
}
\description{
Uses genetic algorithm to find the minima of a given fitness function.
It evolves chromosomes with limited-range integers as codons.
}
\usage{
GeneticAlg.int(genomeLen, codonMin, codonMax, 
    genomeMin = rep.int(codonMin, genomeLen), 
    genomeMax = rep.int(codonMax, genomeLen), 
    suggestions = NULL, popSize = 50, 
    iterations = 100, terminationFitness = NA, 
    mutationChance = 1/(genomeLen+1), elitism = floor(popSize/10), 
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
  over individual codon's minimum. Overrides \code{codonMin}.
}
  \item{genomeMax}{
  A vector of length \code{genomeLen} containing fine-grained control
  over individual codon's maximum. Overrides \code{codonMax}.
}
  \item{suggestions}{
  A list of suggested chromosomes to be used in initial population.
}
  \item{popSize}{
  Size of the population.
}
  \item{iterations}{
  Number of generations to evolve the population.
}
  \item{terminationFitness}{
  Target fitness. If the best chromosome's fitness reaches this value
   the algorithm terminates.
}
  \item{mutationChance}{
  The chance of a codon being mutated. It must be between 0 and 1.
}
  \item{elitism}{
  Number of top ranking chromosomes that are directly transfered to next generation without
  going through evolutionary operations.
}
  \item{monitorFunc}{
  A function that is called at each generation. Can be used to monitor evolution of population.
}
  \item{evalFunc}{
  Fitness function.
}
  \item{allowrepeat}{
  Allows or forbids repeated values in the chromosome.
}
  \item{showSettings}{
  Enables printing GA settings.
}
  \item{verbose}{
  Enables verbose debugging info.
}
  \item{plapply}{
  \code{lapply} function used for mapping chromosomes to fitness function.
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
Fitness of each chromosome is evaluated using the fitness function
\code{evalFunc}. If one of the chromosomes reaches
\code{terminationFitness}, the algorithm terminates; 
Otherwise evolutionary operators including selection, cross-over
and mutation are applied to the population to create a new generation.
This iteration is continued until the required fitness is reached
or the number of generations exceeds \code{iterations}.

At each generation, the supplied \code{monitorFunc} is called, taking a
list similar to \code{GeneticAlg.int} returning value. 

The \code{evalFunc} receives integer sequences and must return a numeric value.
The goal of optimization would be to find a chromosome which minimizes this value.

To parallelize fitness function evaluation, set \code{plapply} to a parallelized
\code{lapply}, such as \code{mclapply} from package \code{parallel}. 
In functions that do not handle data dependencies such as \code{parLapply}, 
variables and functions required for correct execution of \code{evalFunc} 
must be exported to worker nodes before invoking \code{GeneticAlg.int}.
}
\value{

  \item{genomeMin}{
    Minimum of individual codons as used in the function call. }

  \item{genomeMax}{
    Maximum of individual codons as used in the function call. }

  \item{popSize}{
    Size of the population as used in the function call.}

  \item{iterations}{
    Number of maximum generations as used in the function call.}

  \item{suggestions}{
    Suggested chromosomes as used in the function call.}

  \item{elitism}{
    Number of elite individuals as used in the function call.}

  \item{mutationChance}{
    Mutation chance as used in the function call.}

  \item{population}{
    The population during all generations.}

  \item{evaluations}{
    Fitness of latest generation.}

  \item{best}{
    Average fitness of the population.}

  \item{mean}{
    Average fitness of the population.}

  \item{currentIteration}{
      Number of generations evolved until now.}

  \item{bestChrom}{
    The best chromosome in integer sequence format.}
}

\references{
This function is partially based on \code{genalg} package
by Egon Willighagen. See \url{http://cran.r-project.org/web/packages/genalg/}.
}

% \author{
% %%  ~~who you are~~
% }
% \note{
% %%  ~~further notes~~
% }
%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{GrammaticalEvolution}}
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
    cat("Best of gen: ", min(result$evaluations), "\n")
}

x <- GeneticAlg.int(genomeLen = 20, codonMin = 0, codonMax = 20,
                allowrepeat = FALSE, terminationFitness = -109,
                monitorFunc = monitorFunc, evalFunc = evalfunc)

best.result <- x$bestChrom

print(best.result)
print("Odds:")
print(sort(best.result[seq(1, 20, 2)]))
print("Evens:")
print(sort(best.result[seq(2, 20, 2)]))
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
