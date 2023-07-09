# TIME DECOMPOSITION EXPERIMENTAL CODE
# LAST EDITED: 12/08/2021

# IMPORTS
# -------

library(jagsUI)
library(truncnorm)


#
# ****************************** START OF CONFIGURATIONS ******************************
#

working.directory <- paste(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd(working.directory)
source("Functions.R")


result.dir <- "Simulated datasets 6"
create.dir(result.dir, recursive = TRUE)


data.file <- "Tests A-D raw.csv"
test.names <- c("statpak", "IFNgamma", "cult_SUM")


# HYPER-PARAMETERS
# ----------------

# No. diagnostic tests
n.diag <- 3 

# Set JAGS parameter search limits
se.limit <- c(0, 1)
sp.limit <- c(0.5, 1)
pi.limit <- c(0, 0.5)

# What relationship is assumed between prevalence, se, sp and time?
# Can be either "constant", "independent", "linear"
pi.relationship <- "independent"  
se.relationship <- "linear"   
sp.relationship <- "linear"  

scenario.name <- "woodchester_independent_linear_linear"

# JAGS MODEL PARAMETERS
# ---------------------
ni <- 1e5       # (Integer) Number of iterations
nt <- 1         # (Integer) Thinning
nb <- 1e3       # (Integer) Burn-in
nc <- 3         # (Integer) Number of chains
na <- NULL      # (Integer) Adapt (Set to NULL and na will be set automaticlly)

#
# ******************************* END OF CONFIGURATIONS *******************************
#

#  FUNCTION DECLARATIONS
# ----------------------

load.data <- function(data.file, test.names){
  # load data function that includes time
  data <- read.csv(data.file)                    # Get the data into a dataframe from the file
  data <- data[, c(test.names, "date")]          # Remove columns that aren't in test.names or date
  data <- na.omit(data)                          # Omit rows which have NA
  y <- data[, "date"]                            # Make a vector of the dates
  y <- substring(y, 7, 10)                       # Extract the 7th - 10th chars (the year)
  data <- data[, test.names]                     # Make a dataframe of the tests only 
  data[data > 1] <- 1                            # If bigger than 1, make 1
  data["year"] <- as.numeric(y)                  # Add the years to the dataframe
  return(data)
}


data.to.test.array <- function(data, test.names){
  n.tests <- length(test.names)
  year.min <- min(data[, "year"])
  t.steps <- max(data[, "year"]) - year.min + 1
  
  n.badgers <- dim(data)[1]
  test.array <- array(0, dim=c(1, t.steps, 2 ** n.tests))
  for (b in 1:n.badgers){
    y <- data[b, "year"] - year.min + 1
    i <- sum(2**rev(seq(n.tests) - 1) * data[b, test.names]) + 1
    test.array[1, y, i] <- test.array[1, y, i] + 1
  }
  return(test.array)
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


# LOAD DIAGNOSTIC TEST RESULTS
# --------------------------------

data <- load.data(data.file, test.names)
test.array <- data.to.test.array(data, test.names)
n.time <- dim(test.array)[2]



# SETUP
# -----

param.names <- get.param.names(n.diag)


set.model(scenario.name, n.diag, pi.relationship, se.relationship, sp.relationship, FALSE)


jags.data <- list(
  y=test.array[1,,],
  n=rowSums(test.array[1,,]),
  n.diag=n.diag,
  n.time=n.time,
  se.limit=se.limit,
  sp.limit=sp.limit,
  pi.limit=pi.limit
)

# RUN
# ---

output <- jags(
  data=jags.data,
  inits=NULL,
  model.file=paste(scenario.name, "txt", sep="."),
  parameters.to.save=c("pi","se", "sp"),
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

results <- outputs.to.dataframe(output)
results["time.step"] <- seq(n.time)
results <- melt(results, id=c("time.step"))
results["simulation"] <- 1
results["type"] <- "predicted"
results["index"] <- 0
results["parameter"] <- 0
for (i in 1:dim(results)[1]){
  results[i, "index"] <- get.index(results$variable[i], n.diag)
  results[i, "parameter"] <- get.param(results$variable[i])
}


filepath <- paste(paste(result.dir, scenario.name, sep="/"), "rds", sep=".")
saveRDS(results, filepath)
beep()

