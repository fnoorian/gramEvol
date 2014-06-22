GeneticAlg.int <-
function(genomeLen, codonMin, codonMax,
                 genomeMin=rep.int(codonMin, genomeLen), genomeMax=rep.int(codonMax, genomeLen),
                 suggestions=NULL,
                 popSize=50, 
                 iterations=100, terminationFitness=NA,
                 mutationChance=1/(genomeLen+1),
                 elitism=floor(popSize/10),
                 monitorFunc=NULL, 
                 evalFunc,
                 allowrepeat = TRUE,
                 showSettings=FALSE, verbose=FALSE,
                 plapply = lapply) {
    # Optimizes an Integer chromosome using a genetic algorithm.
    #
    # popSize          = the population size
    # iterations       = number of generations
    # terminationFitness = The fitness error that if reached, the GA should termiante
    # mutationChance   = chance that a var in the string gets mutated
    #
    # Partially based on "R Based Genetic Algorithm (genalg package)""
    # http://cran.r-project.org/web/packages/genalg/

    is.verbose = verbose
    verbose = function(...) { if (is.verbose) cat(...)}

    if (is.null(evalFunc)) {
        stop("A evaluation function must be provided. See the evalFunc parameter.");
    }
    
    stopifnot(genomeLen > 1)
    
    # do a variaty of sanity checks first
    verbose("Testing the sanity of parameters...\n");
    if (length(genomeMin) != length(genomeMax)) {
        stop("The vectors genomeMin and genomeMax must be of equal length.");
    }
    if (popSize < 5) {
        stop("The population size must be at least 5.");
    }
    if (iterations < 1) {
        stop("The number of iterations must be at least 1.");
    }
    if (!(elitism < popSize)) {
        stop("The population size must be greater than the elitism.");
    }
    if (elitism < 0) {
        stop("elitism must be at least 0.");
    }
    if ((mutationChance < 0) | (mutationChance  > 1)) {
        stop("mutationChance must be between 0 and 1.");
    }
    
    if (showSettings) {
        verbose("The start conditions:\n");
        result = list(genomeMin=genomeMin, genomeMax=genomeMax, suggestions=suggestions,
                      popSize=popSize, iterations=iterations,
                      elitism=elitism, mutationChance=mutationChance);
        class(result) = "rbga";
        
        cat(summary(result));
    } else {
        verbose("Not showing GA settings...\n");
    }

    ##########
    rand.int <- function(n, mins, maxs) {
        (mins - 1) + sample.int(maxs - mins + 1, n, replace=TRUE)
    }
    unique.maker <- function(x, genomeMin, genomeMax) {
        while (TRUE) {
            dup = duplicated(x)
            if (!any(dup))
                break
            for (i in which(dup)) {
                x[i] = x[i] + 1
                if (x[i] > genomeMax[i]) {
                    x[i] = genomeMin[i]
                }
            }
        }

        return (x)
    }

    ##########
    # Creation
    population = matrix(nrow=popSize, ncol=genomeLen);

    if (!is.null(suggestions)) {
        verbose("Adding suggestions to first population...\n");
        suggestionCount = nrow(suggestions)
        population[1:suggestionCount,] = suggestions
        verbose("Filling others with random values in the given domains...\n");
    } else {
        verbose("Starting with random values in the given domains...\n");
        suggestionCount = 0
    }

    for (var in 1:genomeLen) {
        population[(suggestionCount+1):popSize,var] = rand.int(popSize-suggestionCount, 
            genomeMin[var],
            genomeMax[var])
    }

    if (!allowrepeat) {
        for (i in (suggestionCount+1):popSize) {
            population[i,] = unique.maker(population[i,], genomeMin, genomeMax)
        }
    }
    ############################################################################
    # do iterations
    bestEvals = rep(NA, iterations);
    meanEvals = rep(NA, iterations);
    evalVals = rep(NA, popSize);
    for (iter in 1:iterations) {
        verbose(paste("Starting iteration", iter, "\n"));

        ##########
        # Evaluation

        verbose("Calucating evaluation values... ");

        to.eval.Ids = which(is.na(evalVals))
        evalVals[to.eval.Ids] = unlist(plapply(to.eval.Ids, 
            function(i, population, evalFunc) evalFunc(population[i, ]),
            population, evalFunc))

        # check for invalid items
        if ((!all(is.numeric(evalVals))) |
            any(is.na(evalVals)) |
            any(is.nan(evalVals))) {
            stop("Invalid fitness function return value (NA or NaN).")
        }

        # extract statistics about generation
        bestEvals[iter] = min(evalVals);
        meanEvals[iter] = mean(evalVals);
        verbose(" done.\n");
        
        if (!is.null(monitorFunc)) {
            verbose("Sending current state to rgba.monitor()...\n");
            # report on GA settings
            result = list(genomeMin=genomeMin, genomeMax=genomeMax,
                          popSize=popSize, iterations=iterations, suggestions=suggestions,
                          population=population, elitism=elitism, mutationChance=mutationChance,
                          evaluations=evalVals, best=bestEvals, mean=meanEvals,
                          currentIteration=iter,
                          bestChrom=population[which.min(evalVals),]);
            class(result) = "GeneticAlg.int";
            
            monitorFunc(result);
        }
        
        ##########
        # check termination conditions
        if (iter == iterations) {
            verbose("End of generations iteration reached.\n");
            break
        }

        if (!is.na(terminationFitness)) {
            if (bestEvals[iter] < terminationFitness) {
                verbose("Fitness better than termination fitness reached.\n");
                break
            }
        }
        
        ##########
        # Selection

        verbose("Creating next generation...\n");
        newPopulation = matrix(nrow=popSize, ncol=genomeLen);
        newEvalVals = rep(NA, popSize);
        
        verbose("  sorting results...\n");
        sortedEvaluations = sort(evalVals, index=TRUE);
        sortedPopulation  = matrix(population[sortedEvaluations$ix,], ncol=genomeLen);
        
        # save the best
        if (elitism > 0) {
            verbose("  applying elitism...\n");
            newPopulation[1:elitism,] = sortedPopulation[1:elitism,];
            newEvalVals[1:elitism] = sortedEvaluations$x[1:elitism]
        } # ok, save nothing
        
        ##########
        # Crossover
        # fill the rest by doing crossover
        verbose("  applying crossover...\n");
        for (child in (elitism+1):popSize) {
            # ok, pick two random parents using roulette wheel probability
            parentProb = dnorm(1:popSize, mean=0, sd=(popSize/3))
            parentIDs = sample(1:popSize, 2, prob=parentProb)
            parents = sortedPopulation[parentIDs,]

            crossOverPoint = sample(0:genomeLen,1)

            if (crossOverPoint == 0) {
                newPopulation[child, ] = parents[2,]
                newEvalVals[child] = sortedEvaluations$x[parentIDs[2]]
            } else if (crossOverPoint == genomeLen) {
                newPopulation[child, ] = parents[1,]
                newEvalVals[child] = sortedEvaluations$x[parentIDs[1]]
            } else {
                newPopulation[child, ] = 
                    c(parents[1, 1:crossOverPoint], 
                      parents[2, (crossOverPoint+1):genomeLen])
            }
        }
        
        if (!allowrepeat) {
            for (i in (elitism+1):popSize) {
                population[i,] = unique.maker(population[i,], genomeMin, genomeMax)
            }
        }

        population = newPopulation;
        evalVals   = newEvalVals;
        
        ##########
        # Mutation
        if (mutationChance > 0) {
            verbose("  applying mutations... ");
            mutationCount = 0;
            for (object in (elitism+1):popSize) { # don't mutate the best
                mut_genomeLen = runif(genomeLen) < mutationChance # do mutation for some of variables by chance
                num_muts = sum(mut_genomeLen)
                mutationCount = mutationCount + num_muts

                # OPTION 1
                # mutate to something random
                #mutation = genomeMin[mut_genomeLen] +
                #    runif(num_muts)*(genomeMax[mut_genomeLen]-genomeMin[mut_genomeLen]);
                
                # OPTION 2
                # mutate around solution
                dempeningFactor = (iterations-iter)/iterations
                direction       = sample(c(-1,1), num_muts, replace=TRUE)
                mutationVal     = genomeMax[mut_genomeLen]-genomeMin[mut_genomeLen]*0.67
                mutation = round(population[object,mut_genomeLen] + 
                           direction*mutationVal*dempeningFactor)
                # but in domain. if not, then take random
                bad_mutations = which( (mutation < genomeMin[mut_genomeLen]) | (mutation > genomeMax[mut_genomeLen]) )
                for (b in bad_mutations) {
                    mutation[bad_mutations] = rand.int(n=1, 
                        genomeMin[mut_genomeLen][b],
                        genomeMax[mut_genomeLen][b])
                }
                
                # apply mutation, and delete known evalutation value
                population[object, mut_genomeLen] = mutation;
                if (!allowrepeat) {
                    population[object,] = unique.maker(population[object,], genomeMin, genomeMax)
                }

                evalVals[object] = NA;
                mutationCount = mutationCount + 1;
            }
            verbose(paste(mutationCount, "mutations applied\n"));
        }
    }
    
    # report on GA settings
    result = list(genomeMin=genomeMin, genomeMax=genomeMax,
                  popSize=popSize, iterations=iterations, suggestions=suggestions,
                  population=population, elitism=elitism, mutationChance=mutationChance,
                  evaluations=evalVals, best=bestEvals, mean=meanEvals,
                  currentIteration=iter,
                  bestChrom=population[which.min(evalVals),]);
    class(result) = "GeneticAlg.int";

    return(result);
}
