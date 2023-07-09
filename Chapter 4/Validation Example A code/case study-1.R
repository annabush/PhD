# Anna Bush
# Exeter University
# Latest Update: 07/03/2020

# Evaluating model performance for different numbers of diagnostic tests

# ---------------------------------------- #
#         Load Necessary Libraries         #
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

working.directory <- dirname(rstudioapi::getActiveDocumentContext()$path)

# Set simulation parameters
n.tests <- 4            # (Integers) The numbers of tests to iterate over, e.g.: 1:3
n.sim <- 1               # (Integer) Number times the model is validated with different true values
n.badgers <- 500          # (Integers) The number of bagers per sample
n.cores <- 1              # Set the number of cores to use (Integer)
prior.sd <- 0.05          # (Float) Standard deviation of priors for se and sp in JAGS model
draw.sd <- 0.05

# Set true values
se <- c(0.81, 0.71, 0.66, 0.52, 0.59)
sp <- c(0.51, 0.56, 0.91, 0.94, 0.72)
pi <- 0.5

# Set constraints
pi.limit <- c(0, 1)       # (Integers) Lower and upper limit for prevelance values
se.limit <- c(0, 1)       # (Integers) Lower and upper limit for sensitivity values
sp.limit <- c(0, 1)       # (Integers) Lower and upper limit for specificity values

# Set BUGS model hyper-parameters
ni <- 1e3                 # (Integer) Number of iterations
nt <- 1                   # (Integer) Thinning
nb <- 1e2                # (Integer) Burn-in
nc <- 3                   # (Integer) Number of chains
na <- NULL                # (Ingeter) Adapt (Set to NULL and na will be set automaticlly)

# Seed for random number generators (Integer) - set to NULL for no seed
seed <- 18

# ---------------------------------------- #
#             Define Functions             #
# ---------------------------------------- #

get.names <- function(n.tests) {
  se <- rep(NA, n.tests)
  sp <- rep(NA, n.tests)
  # These data names correspond to those found in the output of the JAGS model
  for (i in 1:max(n.tests)) {
    se[i] <- paste("se", i, sep="")
    sp[i] <- paste("sp", i, sep="")
  }
  return (list(all=c("pi", se, sp), se=se, sp=sp,pi="pi", sesp=c(se, sp)))
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


get.test.results <- function(seed=NULL){
  # Set seed
  if (!is.null(seed)) {
    set.seed(seed)
  }
  # Set array to store resuts for each badger
  raw.data <- array(
    data=NA,
    dim=c(n.badgers, max(n.tests)),
    dimnames=list(badger=1:max(n.badgers), test=1:max(n.tests))
  )
  status <- rbinom(n=max(n.badgers), size=1, p=pi)            # Simulate status of all badgers
  for (test in 1:max(n.tests)){                               # For each test
    p <- status * se[test] + (1 - status) * (1 - sp[test])    # Probability that each badger tests positive
    raw.data[, test] <- rbinom(max(n.badgers), 1, p)          # Store test results in raw.data
  }
  print(head(raw.data, 10))                                   # Print sample of raw.data
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
  print(results)
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



set.model.orig <- function() {
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
  test.results <- get.test.results(seed=sim + seed)
  
  # Make array to store results from a single simulation
  sim.results <- array(
    data=NA,
    dim=c(length(n.tests), 4, 2 * max(n.tests) + 1),
    dimnames=list(
      n.tests=n.tests, 
      statistic=c("true", "mean", "sd", "error"), 
      param=get.names(max(n.tests))$all
      )
  )
  
  # Run for each number of tests
  for (t in 1:length(n.tests)){
    bugs.data <- list(
      y=test.results[[t]],
      n=n.badgers,
      n.tests=n.tests[t],
      outcomes=get.outcome.matrix(n.tests[t]),
      n.outcomes=2**n.tests[t],
      se.limit=se.limit,
      sp.limit=sp.limit,
      pi.limit=pi.limit,
      precision=1 /prior.sd ^ 2,
      prior.se=prior.se[1:n.tests[t]],
      prior.sp=prior.sp[1:n.tests[t]]
    )
    # Run the JAGS MCMC Model
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
      store.data=TRUE,
    )
    saveRDS(output, paste("output_", sprintf("%03d", sim), "_", n.tests[t], "_", date, ".RData", sep=""))
    # Store some of the outputs
    sim.results[t, "true", "pi"] <- pi
    sim.results[t, "true", get.names(n.tests[t])$se] <- se[1:n.tests[t]]
    sim.results[t, "true", get.names(n.tests[t])$sp] <- sp[1:n.tests[t]]
    sim.results[t, "mean", "pi"] <- output$mean$pi
    sim.results[t, "mean", get.names(n.tests[t])$se] <- output$mean$se
    sim.results[t, "mean", get.names(n.tests[t])$sp] <- output$mean$sp
    sim.results[t, "sd","pi"] <- output$sd$pi
    sim.results[t, "sd", get.names(n.tests[t])$se] <- output$sd$se
    sim.results[t, "sd", get.names(n.tests[t])$sp] <- output$sd$sp
    sim.results[t, "error","pi"] <- sim.results[t, "true", "pi"] - sim.results[t, "mean", "pi"]
    sim.results[t, "error", get.names(n.tests[t])$se] <- sim.results[t, "true", get.names(n.tests[t])$se] - sim.results[t, "mean", get.names(n.tests[t])$se]
    sim.results[t, "error", get.names(n.tests[t])$sp] <- sim.results[t, "true", get.names(n.tests[t])$sp] - sim.results[t, "mean", get.names(n.tests[t])$sp]
  }
  return (sim.results)
}


# ---------------------------------------- #
#  Set Seed, Working Directory and Checks  #
# ---------------------------------------- #

# Checks that enough true values have been defined for se and sp
if (max(n.tests) > length(se) | max(n.tests) > length(sp)){
  stop("The maximum number of tests specified by 'n.tests' is greater than the number of tests defined by 'se' and 'sp'")
}

# Set the working directiry
setwd(working.directory)

# Set seed
if (!is.null(seed)) {
  set.seed(seed)
}

# Set date
date <- Sys.Date()

# ---------------------------------------- #
#           Select prior means             #
# ---------------------------------------- #

# Draw prior means from truncated normal distribution where: mean=true value, sd=draw.sd
prior.se <- rtruncnorm(max(n.tests), se.limit[1], se.limit[2], se[1:max(n.tests)], draw.sd)
prior.sp <- rtruncnorm(max(n.tests), sp.limit[1], sp.limit[2], sp[1:max(n.tests)], draw.sd)

# Print true values and prior means
data.frame(
  list(
    varible=get.names(max(n.tests))$sesp,
    true.value=c(se[1:max(n.tests)], sp[1:max(n.tests)]),
    prior.mean=c(prior.se, prior.sp)
  )
)


# ---------------------------------------- #
#                  Example of Test Results #
# ---------------------------------------- #

test.results <- get.test.results()
test.results


# ---------------------------------------- #
#                  Execute                 #
# ---------------------------------------- #

set.model()

# Intialise array to collect simulation outputs in
# 4-dimensions: simulations x tests x statistics x parameters
results <- array(
  data=NA,
  dim=c(n.sim, length(n.tests), 4, 2 * max(n.tests) + 1),
  dimnames=list(
    sim=1:n.sim, 
    n.tests=n.tests, 
    statistic=c("true", "mean", "sd", "error"), 
    param=get.names(max(n.tests))$all
    )
)

# Run simulations over multiple cores
outputs <- mclapply(1:n.sim, run, mc.cores=n.cores)

# Put simulation outputs into results array and save
for (sim in 1:n.sim){
  results[sim,,,] <- outputs[[sim]]
}

saveRDS(results, "results.RData")
results <- melt(results)
print(head(results))

# ---------------------------------------- #
#                  Plots                   #
# ---------------------------------------- #

results <- readRDS("C:/Users/anna-/OneDrive - University of Exeter/Chapter 2/Case Study 1 Results/Case Study 1-original/results.RData")
results <- melt(results)

df <- results[which(results$statistic == "error" & !is.na(match(results$param, c("se1", "sp1", "pi")))),]
df$value <- abs(df$value)
p <- ggplot(df, aes(x=n.tests, y=value, group=param, color=param)) + theme_classic()+
  stat_summary(fun.y=mean, geom="line", size = 0.9, position=position_dodge(width=0.25)) +
  stat_summary(fun.y=mean, geom="point", size=3, position=position_dodge(width=0.25)) +
  stat_summary(
    fun.y=mean,
    fun.ymin=function(x) pmax(mean(x) - sd(x),0), 
    fun.ymax=function(x) mean(x) + sd(x), 
    width=0.1,
    size=0.9,
    geom="errorbar",
    position=position_dodge(width=0.25)
  )+
  ylab("Absolute error")+
  xlab("Number of diagnostic tests") +
  labs(col="Parameter") +
  scale_y_continuous(limits = c(0, NA))
  print(p)
ggsave(filename="TestsvError.png")

# ---------------------------------------- #
#         Prior & Posterior Plots          #
# ---------------------------------------- #

n.samples <- (ni - nb) * nc
for (tests in n.tests) {
  posteriors <- array(
    NA, 
    dim=c(n.samples, n.sim, 3), 
    dimnames=list(mcmc.sample=1:n.samples, sim=1:n.sim, param=c("pi", "se1", "sp1"))
  )
  for (sim in 1:n.sim){
    filename <- paste("output_", sprintf("%03d", sim), "_", tests, "_", date, ".RData", sep="")
    sims.list <- readRDS(filename)$sims.list
    posteriors[, sim, 1] <- sims.list$pi
    posteriors[, sim, 2] <- sims.list$se[, 1]
    posteriors[, sim, 3] <- sims.list$sp[, 1]
  }
  post <- melt(posteriors)
  
  # SENSITIVITY
  p <- ggplot() + theme_classic()+
    labs(title=sprintf("Prior and posterior distributions:\nsensitivity 1\n%d tests", tests), x="Sensitivity", y="Density", color="Legend") +
    geom_density(data=post[which(post$param == "se1"),], aes(x=value, group=sim, color="Posterior"), show.legend=FALSE) +
    stat_density(data=post[which(post$param == "se1"),], aes(x=value, group=sim, color="Posterior"), geom="line", position="identity") +
    stat_function(data=data.frame(x=c(0.6, 1)), aes(x, color="Prior"), fun=dtruncnorm, n=101, args=list(a=se.limit[1], b=se.limit[2], mean=prior.se[1], sd=prior.sd), size=1) +
    geom_vline(aes(xintercept=se[1], colour="Truth"), show.legend=FALSE, size=1) +
    scale_colour_manual(values = c("cornflowerblue", "red", "forestgreen"))
  print(p)
  ggsave(filename=sprintf("SePDF%d.png", tests))
  
  # SPECIFICITY
  p <- ggplot() + theme_classic()+
    labs(title=sprintf("Prior and posterior distributions:\nspecificity 1\n%d tests", tests), x="Specificity", y="Density", color="Legend") +
    geom_density(data=post[which(post$param == "sp1"),], aes(x=value, group=sim, color="Posterior"), show.legend=FALSE) +
    stat_density(data=post[which(post$param == "sp1"),], aes(x=value, group=sim, color="Posterior"), geom="line", position="identity") +
    stat_function(data=data.frame(x=c(0.6, 1)), aes(x, color="Prior"), fun=dtruncnorm, n=101, args=list(a=sp.limit[1], b=sp.limit[2], mean=prior.sp[1], sd=prior.sd), size=1) +
    geom_vline(aes(xintercept=sp[1], color="Truth"), show.legend=FALSE, size=1) +
    scale_colour_manual(values = c("cornflowerblue", "red", "forestgreen"))
  print(p)
  ggsave(filename=sprintf("SpPDF%d.png", tests))
  
  # PREVALENCE
  p <- ggplot() + theme_classic()+
    labs(title=sprintf("Prior and posterior distributions:\nprevalence 1\n%d tests", tests), x="Specificity", y="Density", color="Legend") +
    geom_density(data=post[which(post$param == "pi"),], aes(x=value, group=sim, color="Posterior"), show.legend=FALSE) +
    stat_density(data=post[which(post$param == "pi"),], aes(x=value, group=sim, color="Posterior"), geom="line", position="identity") +
    stat_function(data=data.frame(x=c(0, 1)), aes(x, color="Prior"), fun=dunif, n=101, args=list(min=pi.limit[1], max=pi.limit[2]), size=1) + 
    geom_vline(aes(xintercept=pi, colour="Truth"), show.legend=FALSE, size=1) +
    scale_colour_manual(values = c("cornflowerblue", "red", "forestgreen"))
  print(p)
  ggsave(filename=sprintf("PiPDF%d.png", tests))
}

