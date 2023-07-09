"
Anna Bush

Script to create a panel of probability density plots to show the 
densities of prior and posterior distributions for a simple model.
"

library(ggplot2)
library(reshape2)
library(utils)

working.directory <- dirname(rstudioapi::getActiveDocumentContext()$path)
outputs.directory <- dirname(rstudioapi::getActiveDocumentContext()$path)
results.path <- paste(dirname(rstudioapi::getActiveDocumentContext()$path), "results.RData", sep="/")

# Input required vars
date <- "2020-03-23"
n.samples <- 900000  # 2700000 = (ni - nb) * nc

# Set simulation parameters
n.tests <- 2:5        # (Integers) The numbers of tests to iterate over, e.g.: 1:3
n.sim <- 10           # (Integer) Number times the model is validated with different true values
prior.sd <- 0.05      # (Float) Standard deviation of priors for SE and SP in JAGS model
draw.sd <- 0.05       # (Float) Standard deviation for drawing the mean of prior distributions

# Set true values
se <- c(0.81, 0.71, 0.66, 0.52, 0.59)
sp <- c(0.51, 0.56, 0.91, 0.94, 0.72)
pi <- 0.5

# Set constraints
pi.limit <- c(0, 1)       # (Integers) Lower and upper limit for prevelance values
se.limit <- c(0, 1)       # (Integers) Lower and upper limit for sensitivity values
sp.limit <- c(0, 1)       # (Integers) Lower and upper limit for specificity values

setwd(working.directory)

results <- readRDS(results.path)
results <- melt(results)

df <- results[which(results$statistic == "error" & !is.na(match(results$param, c("se1", "sp1", "pi")))),]
df$value <- abs(df$value)

# Draw prior means from truncated normal distribution where: mean=true value, sd=draw.sd
set.seed(18)
prior.se <- rtruncnorm(max(n.tests), se.limit[1], se.limit[2], se[1:max(n.tests)], draw.sd)
prior.sp <- rtruncnorm(max(n.tests), sp.limit[1], sp.limit[2], sp[1:max(n.tests)], draw.sd)

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

# Print true values and prior means

posteriors <- array(
  NA, 
  dim=c(n.samples, length(n.tests), n.sim, 1), 
  dimnames=list(mcmc.sample=1:n.samples, tests=n.tests, sim=1:n.sim, param=c("se1"))
)

for (t in 1:length(n.tests)) {
  tests <- n.tests[t]
  for (sim in 1:n.sim){
    
    print(sprintf("test: %02d, sim: %02d", tests, sim))
    filename <- paste("output_", sprintf("%03d", sim), "_", tests, "_", date, ".RData", sep="")
    
    sims.list <- readRDS(paste(outputs.directory, filename, sep="/"))$sims.list
    # posteriors[, t, sim, 1] <- sims.list$pi
    posteriors[, t, sim, 1] <- sims.list$se[1:n.samples, 1]
    # posteriors[, t, sim, 3] <- sims.list$sp[, 1]
  }
}
posteriors <- melt(posteriors)

# SENSITIVITY PLOT

# df <- post[which(post$param == "se1"),]
p <- ggplot() + 
  theme_bw() +
  geom_density(data=posteriors, aes(x=value, group=sim, color="Posterior"), show.legend=FALSE) +
  geom_vline(data=posteriors, aes(xintercept=se[1], color="Truth"), show.legend=FALSE) +
  stat_function(data=data.frame(x=c(0.6, 1)), aes(x, color="Prior"), fun=dtruncnorm, n=101, args=list(a=se.limit[1], b=se.limit[2], mean=prior.se[1], sd=prior.sd)) +
  scale_colour_manual(values = c("cornflowerblue", "red", "forestgreen")) +
  labs(x="Sensitivity", y="Density", color="Legend") +
  facet_grid(rows=vars(tests))
p


#labs(title=sprintf("Prior and posterior distributions:\nsensitivity 1\n%d tests", tests), x="Sensitivity", y="Density", color="Legend") +
#geom_density(data=post[which(post$param == "se1"),], aes(x=value, group=sim, color="Posterior"), show.legend=FALSE) +
#  stat_density(data=post[which(post$param == "se1"),], aes(x=value, group=sim, color="Posterior"), geom="line", position="identity") +
  #stat_function(data=data.frame(x=c(0.6, 1)), aes(x, color="Prior"), fun=dtruncnorm, n=101, 
  #              args=list(a=se.limit[1], b=se.limit[2], mean=prior.se[1], sd=prior.sd)) +
  #geom_vline(aes(xintercept=se[1], colour="Truth"), show.legend=FALSE, size=1) +
  #scale_colour_manual(values = c("cornflowerblue", "red", "forestgreen"))+
#  facet_grid(df$n.tests)
#print(p)

