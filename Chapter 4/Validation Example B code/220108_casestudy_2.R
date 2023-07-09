# Anna Bush
# Exeter University
# Latest Update: 01/2022

# Evaluating model performance for different numbers of diagnostic tests

# ---------------------------------------- #
#         Load Necessary Libarries         #
# ---------------------------------------- #

library(jagsUI)
library(coda)
library(gtools)
library(parallel)
library(binaryLogic)
library(stringr)
library(mcmcse)
library(truncnorm)
library(ggplot2)
library(reshape2)

# ---------------------------------------- #
#                 Settings                 #
# ---------------------------------------- #

# Select working directory (Character) - this is where output files will be saved
working.directory <- dirname(rstudioapi::getActiveDocumentContext()$path)

# Set simulation parameters
n.tests <- 2:5                  # (Integers) The numbers of tests to iterate over, e.g.: 1:3
n.sim <- 25                      # (Integer) Number times the model is validated with different true values
n.badgers <- 500                # (Integers) The number of bagers per sample
n.cores <- 5                    # Set the number of cores to use (Integer)
prior.sd <- c(0.05, 0.15)  # (Floats) Standard deviations of priors for se and sp in JAGS model
draw.sd <- 0.05

# Set constraints
pi.limit <- c(0.5, 1)       # (Integers) Lower and upper limit for prevelance values
se.limit <- c(0, 1)       # (Integers) Lower and upper limit for sensitivity values
sp.limit <- c(0, 1)       # (Integers) Lower and upper limit for specificity values

# Set BUGS model hyper-parameters
ni <- 1e6                 # (Integer) Number of iterations
nt <- 1                   # (Integer) Thinning
nb <- 1e5                 # (Integer) Burn-in
nc <- 3                   # (Integer) Number of chains
na <- NULL                # (Ingeter) Adapt (Set to NULL and na will be set automaticlly)

# Seed for random number generators (Integer) - set to NULL for no seed
seed <- 18

# ---------------------------------------- #
#             Define Functions             #
# ---------------------------------------- #


get.values <- function(seed=NULL) {
  # Set seed
  if (!is.null(seed)) {
    set.seed(seed)
  }
  #create an array to first store the mean values
  #which will be the centre of the normal distributions the true values will come from
  names <- get.names(max(n.tests))
  true.values <- array(
    data = c(
      runif(n.sim, pi.limit[1], pi.limit[2]),
      runif(n.sim * max(n.tests), se.limit[1], se.limit[2]),
      runif(n.sim * max(n.tests), sp.limit[1], sp.limit[2])
    ),
    dim = c(n.sim, 2 * max(n.tests) + 1),
    dimnames = list(n.sim=1:n.sim, param=names$all)
  )
  mean.values <- array(
    data = c(
      rtruncnorm(n.sim * max(n.tests), se.limit[1], se.limit[2], true.values[,names$se], draw.sd),
      rtruncnorm(n.sim * max(n.tests), sp.limit[1], sp.limit[2], true.values[,names$sp], draw.sd)
    ),
    dim = c(n.sim, 2 * max(n.tests)),
    dimnames = list(n.sim=1:n.sim, param=names$sesp)
  )
  return(list(mean = mean.values, true = true.values))
}


get.names <- function(n.tests) {
  se <- rep(NA, n.tests)
  sp <- rep(NA, n.tests)
  # These data names correspond to those found in the output of the JAGS model
  for (i in 1:max(n.tests)) {
    se[i] <- paste("se", i, sep="")
    sp[i] <- paste("sp", i, sep="")
  }
  return (list(all=c("pi", se, sp), sesp=c(se, sp), se=se, sp=sp,pi="pi"))
}

get.result.names <- function(tests){
  sapply(0:(2^tests - 1), int2binchar, bits=tests)
}


get.outcome.matrix <- function(tests) {
  # Initialise the outcomes matrix with NA
  outcomes <- matrix(
    data=NA, 
    nrow=2^tests,
    ncol=tests
  )
  # Populate all possible outcomes as a factorial array. 0=negative, 1=positive
  for(i in 1:tests) {
    outcomes[,i] <- rep(rep(c(0, 1), each=nrow(outcomes)/(2^i)), 2^(i-1))
  }
  return (outcomes)
}



int2binchar <- function(n, bits=0) {
  return (paste(as.binary(n, n=bits), collapse=""))
}

get.test.results <- function(pi, se, sp, seed=NULL){
  # Set seed
  if (!is.null(seed)) {
    set.seed(seed)
  }
  # Set array to store resuts for each badger
  raw.data <- array(
    data=NA,
    dim=c(max(n.badgers), max(n.tests)),
    dimnames=list(badger=1:n.badgers, test=1:max(n.tests))
  )
  status <- rbinom(n=max(n.badgers), size=1, p=pi)            # Simulate status of all badgers
  for (test in 1:max(n.tests)){                               # For each test
    p <- status * se[test] + (1 - status) * (1 - sp[test])    # Probability that each badger tests positive
    raw.data[, test] <- rbinom(max(n.badgers), 1, p)          # Store test results in raw.data
  }
  results <- list()                                           # This will store a list of the test arrays   
  for (tests in n.tests){                                     # For each number of tests
    # Make a test array: i.e. (000, 001, 010, 011, ..., 111)
    test.array <- array(0, dim=c(2 ** tests), dimnames=list(result=get.result.names(tests)))
    for (badger in 1:n.badgers){
      out <- paste(raw.data[badger, 1:tests], collapse="")
      test.array[out] <- test.array[out] + 1
    }
    results[[paste(tests, "tests")]] <- test.array
  }
  return(results)
}


set.model <- function() {
  writeLines("model{
  for (i in 1:n.tests) {
    se[i] <- mu.se[i]
    sp[i] <- mu.sp[i]
  }
  pi <- mu.pi
  for (i in 1:n.outcomes) {
    for (j in 1:n.tests) {
      # A = se if badger is positive, 1 - se otherwise
      # B = 1 - sp if badger is positive, sp otherwise
      A[i, j] <- outcomes[i, j] * se[j] + (1 - outcomes[i, j]) * (1 - se[j])
      B[i, j] <- outcomes[i, j] * (1 - sp[j]) + (1 - outcomes[i, j]) * sp[j]
    }
    p[i] <- pi * prod(A[i, 1:n.tests]) + (1 - pi) * prod(B[i, 1:n.tests])
  } 
  y[1:n.outcomes] ~ dmulti(p[1:n.outcomes], n) 
  for (i in 1:n.tests) {
     mu.se[i] ~ dnorm(prior.se[i], precision) T(se.limit[1], se.limit[2])
     mu.sp[i] ~ dnorm(prior.sp[i], precision) T(se.limit[1], sp.limit[2])
  }
  mu.pi ~ dunif(pi.limit[1], pi.limit[2])
}", con="model.txt")
}


run <- function(sim){
  # Get simulated badger test results
  test.results <- get.test.results(
    pi=values$true[sim, "pi"], 
    se=values$true[sim, get.names(max(n.tests))$se], 
    sp=values$true[sim, get.names(max(n.tests))$sp], 
    seed=seed + sim
  )
  
  # Make array to store results from a single simulation
  sim.results <- array(
    data=NA,
    dim=c(length(prior.sd), length(n.tests), 4, 2 * max(n.tests) + 1),
    dimnames=list(
      prior.sd=prior.sd,
      n.tests=n.tests, 
      statistic=c("true", "mean", "sd", "error"), 
      param=get.names(max(n.tests))$all
    )
  )
  
  for (p in 1:length(prior.sd)){
    for (t in 1:length(n.tests)){
      names <- get.names(n.tests[t])
      bugs.data <- list(
        y=test.results[[t]],
        n=n.badgers,
        n.tests=n.tests[t],
        outcomes=get.outcome.matrix(n.tests[t]),
        n.outcomes=2**n.tests[t],
        se.limit=se.limit,
        sp.limit=sp.limit,
        pi.limit=pi.limit,
        precision=1/prior.sd[p]^2,
        prior.se=values$mean[sim, names$se],
        prior.sp=values$mean[sim, names$sp]
      )
      output <- jags(
        data=bugs.data,
        inits=NULL,
        model.file="model.txt",
        parameters.to.save=c("pi","se", "sp"),
        n.adapt=na,
        n.chains=nc, 
        n.thin=nt,
        n.iter=ni, 
        n.burnin=nb,
        store.data=TRUE
      )
      saveRDS(output, paste("output_", sprintf("%03d", sim), "_", n.tests[t], "_", Sys.Date(), ".RData", sep=""))
      # Store some of the outputs
      sim.results[p, t, "true", "pi"] <- values$true[sim, "pi"]
      sim.results[p, t, "true", get.names(n.tests[t])$se] <- values$true[sim, names$se]
      sim.results[p, t, "true", get.names(n.tests[t])$sp] <- values$true[sim, names$sp]
      sim.results[p, t, "mean", "pi"] <- output$mean$pi
      sim.results[p, t, "mean", get.names(n.tests[t])$se] <- output$mean$se
      sim.results[p, t, "mean", get.names(n.tests[t])$sp] <- output$mean$sp
      sim.results[p, t, "sd","pi"] <- output$sd$pi
      sim.results[p, t, "sd", get.names(n.tests[t])$se] <- output$sd$se
      sim.results[p, t, "sd", get.names(n.tests[t])$sp] <- output$sd$sp
      sim.results[p, t, "error","pi"] <- values$true[sim, "pi"] - output$mean$pi
      sim.results[p, t, "error", get.names(n.tests[t])$se] <- values$true[sim, names$se] - output$mean$se
      sim.results[p, t, "error", get.names(n.tests[t])$sp] <- values$true[sim, names$sp] - output$mean$sp
    }
  }
  return (sim.results)
}



# ---------------------------------------- #
#                  Execute                 #
# ---------------------------------------- #

setwd(working.directory)

values <- get.values(seed=seed)
values

set.model()

outputs <- mclapply(1:n.sim, run, mc.cores=n.cores)

# Intialise array to collect simulation outputs in
# 4-dimensions: simulations x tests x statistics x parameters
results <- array(
  data=NA,
  dim=c(n.sim, length(prior.sd), length(n.tests), 4, 2 * max(n.tests) + 1),
  dimnames=list(
    sim=1:n.sim, 
    prior.sd=paste(prior.sd),
    n.tests=n.tests, 
    statistic=c("true", "mean", "sd", "error"), 
    param=get.names(max(n.tests))$all
  )
)
# Put simulation outputs into results array and save
for (sim in 1:n.sim){
  results[sim,,,,] <- outputs[[sim]]
}

saveRDS(results, "results.RData")

