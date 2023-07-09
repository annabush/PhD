# Anna Bush
# Exeter University
# Latest Update: July 21

# IN THIS CODE, TRUTHS ARE VARIED ACROSS PARAMETER SPACE SYSTEMATICALLY, NOT RANDOMLY.

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

# Select working directory - this is where output files will be saved
working.directory <- paste(
  dirname(rstudioapi::getActiveDocumentContext()$path),
  "../../Results/Simulated dataset 5 - study 2 - unconstrained space - same truths/uniform_uncon",
  sep="/"
)    
       

# Set simulation hyper-parameters
n.tests <- 2:5                      # (Integers) The numbers of tests to iterate over, e.g.: 1:3
n.badgers <- c(500, 1000, 1500)                 # (Integers) The number of bagers per sample
draw.sd <- 0.05

# Set true values for tests 2:5
se <- c(0.71, 0.66, 0.52, 0.59)
sp <- c(0.56, 0.91, 0.94, 0.72)

pi.prior.limit <- c(0, 1)            # (Integers) Lower and upper limit for prevelance values
se.prior.limit <- c(0, 1)                 # (Integers) Lower and upper limit for sensitivity values
sp.prior.limit <- c(0, 1)               # (Integers) Lower and upper limit for specificity values

# True values for pi, se1, sp1
pi.true <- seq(0.05, 0.95, 0.1)
se.1 <- seq(0.05, 0.95, 0.1)
sp.1 <- seq(0.05, 0.95, 0.1)
n.cell <- 10  # number of simulations within each 'cell'
n.sim <- n.cell * length(pi.true) * length(se.1) * length(sp.1)

pi.true.limit <- c(0, 1)            # (Integers) Lower and upper limit for prevelance values
se.true.limit <- c(0, 1)                 # (Integers) Lower and upper limit for sensitivity values
sp.true.limit <- c(0, 1)               # (Integers) Lower and upper limit for specificity values

n.cores <- 20                        # (Integer) Number of cores to use
seed <- 62                          # (Integer) Seed for random number generators - set to NULL for no seed

# Set BUGS model parameters
ni <- 1e6                           # (Integer) Number of iterations
nt <- 1                             # (Integer) Thinning
nb <- 1e5                           # (Integer) Burn-in
nc <- 3                             # (Integer) Number of chains
na <- NULL                          # (Ingeter) Adapt (Set to NULL and na will be set automaticlly)


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
      rep(pi.true, each=length(se.1) * length(sp.1) * n.cell), # true pi (systematic)
      #runif(n.sim, pi.true.limit[1], pi.true.limit[2]), # true pi (random)
      rep(rep(se.1, each=length(sp.1) * n.cell), times=length(pi.true)), # se1 (systematic)
      #runif(n.sim, se.true.limit[1], se.true.limit[2]), # se1 (random)
      rep(se, 1, each=n.sim),                 # rest of se
      rep(rep(rep(sp.1, each=n.cell), times=length(se.1)), times=length(pi.true)), # sp1 (systematic)
      # runif(n.sim, sp.true.limit[1], sp.true.limit[2]), # sp1 (random)
      rep(sp, 1, each=n.sim)                  # rest of sp
    ),
    dim = c(n.sim, 1 + 2 * max(n.tests)),
    dimnames = list(n.sim=1:n.sim, param=names$all)
  )
  mean.values <- array(
    data = c(
      rtruncnorm(n.sim * max(n.tests), a=se.true.limit[1], b=se.true.limit[2], mean=true.values[, names$se], sd=draw.sd), # se prior mean
      rtruncnorm(n.sim * max(n.tests), a=sp.true.limit[1], b=sp.true.limit[2], mean=true.values[, names$sp], sd=draw.sd)  # sp prior mea
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


int2binchar <- function(n, bits=0) {
  return (paste(as.binary(n, n=bits), collapse=""))
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


get.test.results <- function(pi, se, sp, seed=NULL){
  # Set seed
  if (!is.null(seed)) {
    set.seed(seed)
  }
  # Set array to store resuts for each badger
  raw.data <- array(
    data=NA,
    dim=c(max(n.badgers), max(n.tests)),
    dimnames=list(badger=1:max(n.badgers), test=1:max(n.tests))
  )
  status <- rbinom(n=max(n.badgers), size=1, p=pi)            # Simulate status of all badgers
  for (test in 1:max(n.tests)){                               # For each test
    p <- status * se[test] + (1 - status) * (1 - sp[test])    # Probability that each badger tests positive
    raw.data[, test] <- rbinom(max(n.badgers), 1, p)          # Store test results in raw.data
  }
  results <- list()                                           # This will store a list of the test arrays   
  for (tests in n.tests){                                     # For each number of tests
    test.array <- array(                                      # The test array has a row for each number of badgers
      0,
      dim=c(length(n.badgers), 2 ** tests),
      dimnames=list(n.badgers=n.badgers, result=get.result.names(tests))
    )
    for (i in 1:length(n.badgers)){
      for (badger in 1:n.badgers[i]){
        out <- paste(raw.data[badger, 1:tests], collapse="")
        test.array[i, out] <- test.array[i, out] + 1
      }
    }
    results[[paste(tests, "tests")]] <- test.array
  }
  return(results)
}


compile.data <- function(params, bugs.params, values){
  names <- get.names(max(n.tests))
  data <- list()
  for (sim in 1:n.sim){
    data[[sim]] <- list(
      sim=sim, 
      seed=seed + sim,
      pi = values$true[sim, "pi"],
      se = values$true[sim, names$se],
      sp = values$true[sim, names$sp]
    )
  }
  return (data)
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
    mu.se[i] ~ dunif(se.limit[1], se.limit[2])
    mu.sp[i] ~ dunif(sp.limit[1], sp.limit[2])
  }
  mu.pi ~ dunif(pi.limit[1], pi.limit[2])
}", con="model.txt")
}


run <- function(data){
  # Get simulated badger test results
  test.results <- get.test.results(pi=data$pi, se=data$se, sp=data$sp, seed=data$seed)
  # Make array to store results from a single simulation
  sim.results <- array(
    data=NA,
    dim=c(length(n.badgers), length(n.tests), 4, 2 * max(n.tests) + 1),
    dimnames=list(
      n.badgers=n.badgers,
      n.tests=n.tests, 
      statistic=c("true", "mean", "sd", "error"), 
      param=get.names(max(n.tests))$all
    )
  )
  
  # Run for each number of tests for each number of badgers
  for (b in 1:length(n.badgers)){
    for (t in 1:length(n.tests)){
      names <- get.names(n.tests[t])
      bugs.data <- list(
        y=test.results[[t]][b, ],
        n=n.badgers[b],
        n.tests=n.tests[t],
        outcomes=get.outcome.matrix(n.tests[t]),
        n.outcomes=2**n.tests[t],
        se.limit=se.prior.limit,
        sp.limit=sp.prior.limit,
        pi.limit=pi.prior.limit
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
      # saveRDS(output, paste("output_", data$sim, "_", n.badgers[b], "_", n.tests[t], "_", Sys.Date(), ".RData", sep=""))
      saveRDS(c(), paste("output_", data$sim, "_", n.badgers[b], "_", n.tests[t], "_", Sys.Date(), ".RData", sep=""))
      # Store some of the outputs
      sim.results[b, t, "true", "pi"] <- values$true[data$sim,"pi"]
      sim.results[b, t, "true", get.names(n.tests[t])$se] <- values$true[data$sim, names$se]
      sim.results[b, t, "true", get.names(n.tests[t])$sp] <- values$true[data$sim, names$sp]
      sim.results[b, t, "mean", "pi"] <- output$mean$pi
      sim.results[b, t, "mean", get.names(n.tests[t])$se] <- output$mean$se
      sim.results[b, t, "mean", get.names(n.tests[t])$sp] <- output$mean$sp
      sim.results[b, t, "sd","pi"] <- output$sd$pi
      sim.results[b, t, "sd", get.names(n.tests[t])$se] <- output$sd$se
      sim.results[b, t, "sd", get.names(n.tests[t])$sp] <- output$sd$sp
      sim.results[b, t, "error","pi"] <- sim.results[b, t, "true", "pi"] - sim.results[b, t, "mean", "pi"]
      sim.results[b, t, "error", get.names(n.tests[t])$se] <- sim.results[b, t, "true", get.names(n.tests[t])$se] - sim.results[b, t, "mean", get.names(n.tests[t])$se]
      sim.results[b, t, "error", get.names(n.tests[t])$sp] <- sim.results[b, t, "true", get.names(n.tests[t])$sp] - sim.results[b, t, "mean", get.names(n.tests[t])$sp]
    }
  }
  return (sim.results)
}
# ---------------------------------------- #
#  Set Seed, Working Directory and Checks  #
# ---------------------------------------- #

# Set the working directiry
setwd(working.directory)

# Set seed
if (!is.null(seed)) {
  set.seed(seed)
}

# ---------------------------------------- #
#                  Execute                 #
# ---------------------------------------- #

set.model()

# Intialise array to collect simulation outputs in
# 4-dimensions: simulations x tests x statistics x parameters
results <- array(
  data=NA,
  dim=c(n.sim, length(n.badgers), length(n.tests), 4, 2 * max(n.tests) + 1),
  dimnames=list(
    sim=1:n.sim, 
    n.badgers=n.badgers,
    n.tests=n.tests, 
    statistic=c("true", "mean", "sd", "error"), 
    param=get.names(max(n.tests))$all
  )
)

# Get mean and true values
values <- get.values()

# Compile data into a list for simulation
data <- compile.data(params, bugs.params, values)

# Run simulations over multiple cores
outputs <- mclapply(data, run, mc.cores=n.cores)

# Put simulation outputs into results array and save
for (sim in 1:n.sim){
  results[sim,,,,] <- outputs[[sim]]
}

saveRDS(results, "results.RData")
