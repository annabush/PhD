# TIME DECOMPOSITION EXPERIMENTAL CODE
# LAST EDITED: 01/04/2023

# Scenario 1 - 'random' - COMPLETE
# ----------
# Values of all parameters are constant through time,
# generated by 'random'
#
#   3 models are used to estimate parameters
#     - independent - 'random_independent'
#     - constant - 'random_constant'
#     - linear -  'random_linear'

# Scenario 2 - 'constant' - COMPLETE
# ----------
# Values of all parameters are constant through time,
# generated by 'constant.random'
#
#   3 models are used to estimate parameters
#     - independent - 'constant_independent'
#     - constant - 'constant_constant'
#     - linear -  'constant_linear'

# Scenario 3 - 'constant-noisy' - COMPLETE
# ----------
# Values of all parameters are constant through time with noise,
# generated by 'constant.random.noisy' with sd=0.02
#  
#   3 models are used to estimate parameters
#     - independent - 'constant-noisy-02_independent'
#     - constant - 'constant-noisy-02_constant'
#     - linear - 'constant-noisy-02_linear'

# Scenario 4 - 'linear' - COMPLETE
# ----------
# Values of all parameters change linearly through time,
# generated by 'linear.random' 
#  
#   3 models are used to estimate parameters
#     - independent - 'linear_independent'
#     - constant - 'linear_constant'
#     - linear - 'linear_linear'

# Scenario 5 - 'linear-noisy' - COMPLETE
# ----------
# Values of all parameters change linearly through time with noise,
# generated by 'linear.random' 
#  
#   3 models are used to estimate parameters
#     - independent - 'linear-noisy-02_independent'
#     - constant - 'linear-noisy-02_constant'
#     - linear - 'linear-noisy-02_linear'

# Scenario 6 - 'mixed' - TODO
# ----------
# prevalence has a noisy linear relationship through time
# se and sp have a noisy constant relationship with time
#  
#   3 models are used to estimate parameters
#     - independent - 'mixed_independent'
#     - constant - 'mixed_constant'
#     - linear - 'mixed_linear'
#     - linear - 'mixed_mixed'

# Scenario 7 - 'timesteps' - TODO
# ----------
# Evaluate the effect of timesteps on the mixed model
# prevalence has a noisy linear relationship through time
# se and sp have a noisy constant relationship with time
#  
#   3 models are used to estimate parameters
#     - 3 timesteps - mixed-3-mixed'
#     - 4 timesteps - 'mixed-4-mixed'
#     - 5 timesteps - 'mixed-mixed'
#     - 6 timesteps - 'mixed-6-mixed'
#     - 7 timesteps - 'mixed-7-mixed'
#     - 8 timesteps - 'mixed-8-mixed'


# IMPORTS
# -------

library(jagsUI)
library(truncnorm)
library(reshape2)
library(ggplot2)
library(beepr)

#
# ****************************** START OF CONFIGURATIONS ******************************
#

working.directory <- paste(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd(working.directory)
source("Functions.R")

result.dir <- "Simulated datasets 6"
create.dir(result.dir, recursive = TRUE)


# HYPER-PARAMETERS
# ----------------

# Scenario name
scenario.name <- "mixed-noisy002_mixed_01"

# No. time steps
n.time <- 1

# No. diagnostic tests
n.diag <- 3 

# Sample size (No. badgers)
n.badgers <- 300

# No. simulations 
n.sim <- 50

# What relationship is assumed between prevalence, se, sp and time?
# Can be either "constant", "independent", "linear"
pi.relationship <- "linear"  
se.relationship <- "constant"    
sp.relationship <- "constant"  

# Set JAGS parameter search limits
se.limit <- c(0, 1)
sp.limit <- c(0.5, 1)
pi.limit <- c(0, 0.5)

# TRUE VALUE GENERATOR OPTIONS: 
# -----------------------------
# > "constant", n, value
#       - value is repeated n times.
#
# > "random.constant", n, lower, upper
#       - a value is drawn from a uniform distribution bounded by lower and upper and then repeated n times.
#
# > "constant.noisy", n, value, mean=0, sd=0.02
#       - value is repeated n times before noise, drawn from a Normal distribution given mean and sd, is applied.
#
# > "linear", n, lower, upper
#       - linear sequence of n values from lower to upper 
#
# > "noisy.linear", n, lower, upper, mean=0, sd=0.02
#       - linear sequence of n values from lower to upper plus noise drawn from a Normal distribution given mean and sd

noise.sd <- 0.02
max.range <- 0.25


# Set prevalence generator
pi.mode <- list("linear.random.noisy", n=n.time, lower=0, upper=0.5, sd=noise.sd, max.range=max.range)

# Set sensitivity generators
se.mode <- list(
  se1=list("constant.random.noisy", n=n.time, lower=0, upper=1, sd=noise.sd),
  se2=list("constant.random.noisy", n=n.time, lower=0, upper=1, sd=noise.sd),
  se3=list("constant.random.noisy", n=n.time, lower=0, upper=1, sd=noise.sd)
)

# Set specificity generators
sp.mode <- list(
  sp1=list("constant.random.noisy", n=n.time, lower=0.5, upper=1, sd=noise.sd),
  sp2=list("constant.random.noisy", n=n.time, lower=0.5, upper=1, sd=noise.sd),
  sp3=list("constant.random.noisy", n=n.time, lower=0.5, upper=1, sd=noise.sd)
)


# JAGS MODEL PARAMETERS
# ---------------------
ni <- 1e5       # (Integer) Number of iterations
nt <- 1         # (Integer) Thinning
nb <- 1e3       # (Integer) Burn-in
nc <- 3         # (Integer) Number of chains
na <- NULL      # (Integer) Adapt (Set to NULL and na will be set automatically)

#
# ******************************* END OF CONFIGURATIONS *******************************
#

#  FUNCTION DECLARATIONS
# ----------------------

constant <- function(n, value){
  return (rep(value, n))
}


random <- function(n, lower, upper){
  return (runif(n, lower, upper))
}


constant.random <- function(n, lower, upper){
  return (rep(runif(1, lower, upper), n))
}


constant.noisy <- function(n, value, mean=0, sd=0.02, lower=0, upper=1){
  return (clip(constant(n, value) + rnorm(n, mean, sd), lower, upper))
}


constant.random.noisy <- function(n, lower, upper, mean=0, sd=0.02){
  return(clip(constant.random(n, lower, upper) + rnorm(n, mean, sd), lower, upper))
}


linear <- function(n, v1, v2){
  return (seq(v1, v2, length=n))
}


linear.random <- function(n, lower, upper, max.range=NA){
  if (is.na(max.range)){
    max.range <- upper - lower
  }
  v1 <- runif(1, lower, upper)
  v2 <- runif(1, max(lower, v1 - max.range), min(upper, v1 + max.range))
  return (linear(n=n, v1, v2))
}


linear.noisy <- function(n, v1, v2, mean=0, sd=0.02){
  return (clip(linear(n, v1, v2) + rnorm(n, mean, sd), min(c(v1, v2)), max(c(v1, v2))))
}


linear.random.noisy <- function(n, lower, upper, max.range=NA, mean=0, sd=0.02){
  return (clip(linear.random(n, lower, upper, max.range) + rnorm(n, mean, sd), lower, upper))
}


clip <- function(vec, lower, upper) {
  return(pmax(pmin(vec, upper), lower))
}


get.test.array <- function(n, pi, se, sp, seed=NULL){
  if (!is.null(seed)){set.seed(seed)}                 # Set seed
  n.diag <- min(length(se), length(sp))              # Infer number of tests
  status <- rbinom(n=n, size=1, p=pi)                 # Simulate infection status of all individuals
  raw.data <- array(NA, c(n, n.diag))                # Array for data storage
  for (i in 1:n.diag){                               # For each test
    p <- status * se[i] + (1 - status) * (1 - sp[i])  # Probability that each badger tests positive
    raw.data[, i] <- rbinom(n, 1, p)                  # Conduct test and store result
  }
  raw.data <- raw.data * array(rep(rep(2, n.diag) ^ c((n.diag - 1):0), each=n), c(n, n.diag))
  raw.data <- rowSums(raw.data)       
  test.array <- array(NA, c(1, 2^n.diag))            # Initialise test array
  for (i in 1:2^n.diag){
    test.array[1, i] <- length(which(raw.data == i-1))
  }
  return(test.array)
}



# SETUP
# -----

generators <- list(
  constant=constant,
  constant.random=constant.random,
  constant.noisy=constant.noisy,
  constant.random.noisy=constant.random.noisy,
  random=random,
  linear=linear,
  linear.random=linear.random,
  linear.noisy=linear.noisy,
  linear.random.noisy=linear.random.noisy
)

param.names <- get.param.names(n.diag)

# INITIALISE GROUND TRUTH VALUES
# ------------------------------
set.seed(97)
true.values <- array(
  0, 
  c(n.sim, n.time, (n.diag * 2 + 1)), 
  list("Simulation"=1:n.sim, "Time step"=1:n.time, "Parameter"=param.names$all)
)
for (sim in 1:n.sim){
  true.values[sim, , "pi"] <- do.call(generators[[pi.mode[[1]]]], pi.mode[c(2:length(pi.mode))])
  
  for (i in 1:n.diag){
    true.values[sim, , param.names$se[i]] <- do.call(generators[[se.mode[[i]][[1]]]], se.mode[[i]][c(2:length(se.mode[[i]]))])
  }
  
  for (i in 1:n.diag){
    true.values[sim, , param.names$sp[i]] <- do.call(generators[[sp.mode[[i]][[1]]]], sp.mode[[i]][c(2:length(sp.mode[[i]]))])
  }
}


# INITIALISE PRIOR MEANS
# ----------------------

set.seed(12)

# SIMULATE DIAGNOSTIC TEST RESULTS
# --------------------------------

set.seed(19)
test.array <- array(NA, c(n.sim, n.time, 2**n.diag))
for (i in 1:n.sim){
  for (j in 1:n.time){
    test.array[i, j, ] <- get.test.array(
      n=n.badgers, 
      pi=true.values[i, j, "pi"], 
      se=true.values[i, j, param.names$se], 
      sp=true.values[i, j, param.names$sp]
    )
  }
}
rm(i, j)

# RUN
# ---

set.model(scenario.name, n.diag, pi.relationship, se.relationship, sp.relationship)

# Array to store errors
errors <- array(
  0, 
  dim=c(n.sim, n.diag * 2 + 4),
  dimnames=list(sim=1:n.sim, error=c(param.names$all, "se", "sp", "total"))
)


results <- data.frame(matrix(ncol=7, nrow=0))
colnames(results) <- c(
  "simulation", 
  "time.step", 
  "type", 
  "variable", 
  "value", 
  "index", 
  "parameter"
)
for (sim in 1:n.sim){
  
  # Set jags data
  jags.data <- list(
    y=test.array[sim,,],
    n=n.badgers,
    n.diag=n.diag,
    n.time=n.time,
    se.limit=se.limit,
    sp.limit=sp.limit,
    pi.limit=pi.limit
  )
  
  # Run jags model
  output <- jags(
    data=jags.data,
    inits=NULL,
    model.file=paste(scenario.name, "txt", sep="."),
    parameters.to.save=c("pi", "se", "sp"),
    n.adapt=na,
    n.chains=nc, 
    n.thin=nt,
    n.iter=ni, 
    n.burnin=nb,
    store.data=TRUE
  )
  
  output <- interpolate.outputs(
    output,
    pi.relationship, 
    se.relationship, 
    sp.relationship, 
    n.diag, 
    n.time
  )
  
  result <- combine.truth.and.predictions(
    true=data.frame(true.values[sim, , ]),
    predicted=outputs.to.dataframe(output)
  )
  result["simulation"] <- sim
  results <- rbind(results, result)
}


filepath <- paste(paste(result.dir, scenario.name, sep="/"), "rds", sep=".")
saveRDS(results, filepath)
beep()
