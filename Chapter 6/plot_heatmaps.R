# ANNA BUSH 
# CODE TO MELT RESULTS PRIOR TO REGRESSION ANALYSES

library(reshape)
library(plyr)
library(Hmisc)
library(boot)
library(base)
library(dplyr)
library(stringr)

data.directory <- paste(dirname(rstudioapi::getActiveDocumentContext()$path), "../Chapter 5/Simulated dataset 3", sep="/")

# Define logit error
logit.error <- function(truth, pred) {
  return(log(pred/(1 - pred)) - log(truth / (1 - truth)))
}

# Define special melt (for normal results)

special.melt <- function(data) {
  rows <- dim(data)[1]
  # Make a list of new column names for errors
  new.names <- c()
  for (name in dimnames(data)$param){
    new.names <- c(new.names, paste("error" , name, sep="."))
  }
  # Initialise an empty dataframe to put results in
  results <- data.frame(
    array(
      NA, 
      dim=c(0, 2 * dim(data)[6] + 34),
      dimnames=list(sim=c(), param=c("sim", "n.tests", "prior.sd", "n.badgers", dimnames(data)$param, new.names, "m.pi", "m.se1", "m.sp1", "error.se", "error.sp", 
                                     "true.se", "true.sp", "error", "sd.se1" , "sd.sp1", "sd.pi", "sd.posterior", "precision",
                                     "error2.pi", "error2.se1", "error2.sp1", "error2", "error3.pi", "error3.se1", "error3.sp1", "error3",
                                     "error4.pi", "error4.se1", "error4.sp1", "error4", "error5.pi", "error5.se1", "error5.sp1", "error5",
                                     "extreme"))
    )
  )
  for (t in dimnames(data)$n.tests){
    for (p in dimnames(data)$prior.sd){
      for (b in dimnames(data)$n.badgers){
        
        
        truth <- data[, p, b, t, "true", ]  # Get array of truth
        error <- data[, p, b, t, "error", ]  # Get array of errors and rename columns
        std <- data[, p, b, t, "sd", ]  # Get array of POSTERIOR standard deviations
        m <- data[, p, b, t, "mean", ]  #the mean in this context is the prediction
        
        dimnames(error)$param <- new.names
        df <- data.frame(cbind(truth, error))
        
        
        # Add sim and n.tests data
        df["sim"] <- 1:rows
        df["n.tests"] <- as.numeric(t)
        df["n.badgers"] <- as.numeric(b)
        
        # mean
        df["m.pi"] <- as.numeric(m[, "pi"])
        df["m.se1"] <- as.numeric(m[, "se1"])
        df["m.sp1"] <- as.numeric(m[, "sp1"])
        
        
        df["error.se"] <- rowMeans(error[, grepl("error.se", colnames(error))], na.rm=TRUE)
        df["error.sp"] <- rowMeans(error[, grepl("error.sp", colnames(error))], na.rm=TRUE)
        df["true.se"] <- as.numeric(truth[, "se1"], na.rm=TRUE)
        df["true.sp"] <- as.numeric(truth[, "sp1"], na.rm=TRUE)
        df["error"] <- rowMeans(error[, c("error.pi", "error.se1", "error.sp1")], na.rm=TRUE)
        df["prior.sd"] <- as.numeric(p)
        df["sd.se1"] <- std[, "se1"]
        df["sd.sp1"] <- std[, "sp1"]
        df["sd.pi"] <- std[, "pi"]
        df["sd.posterior"] <- rowMeans(std[, c("se1", "sp1", "pi")], na.rm=TRUE)
        df["precision"] <- 1 / df["sd.posterior"]^2
        # Error 2 = logit error no abs
        df["error2.pi"] <- logit.error(truth[, "pi"], m[, "pi"])
        df["error2.se1"] <- logit.error(truth[, "se1"], m[, "se1"])
        df["error2.sp1"] <- logit.error(truth[, "sp1"], m[, "sp1"])
        df["error2"] <- rowMeans(df[c("error2.pi", "error2.se1", "error2.sp1")])
        # Error 3 abs(logit.error)
        df["error3.pi"] <- abs(df[["error2.pi"]]) 
        df["error3.se1"] <- abs(df[["error2.se1"]])
        df["error3.sp1"] <- abs(df[["error2.sp1"]])
        df["error3"] <- rowMeans(df[c("error3.pi", "error3.se1", "error3.sp1")])
        # Error 4 = sigma(error3)
        df["error4.pi"] <- inv.logit(df[["error3.pi"]])
        df["error4.se1"] <- inv.logit(df[["error3.se1"]])
        df["error4.sp1"] <- inv.logit(df[["error3.se1"]])
        df["error4"] <- rowMeans(df[c("error4.pi", "error4.se1", "error4.sp1")])
        # Error 5 = absolute error
        df["error5.pi"] <- abs(df[["error.pi"]]) 
        df["error5.se1"] <- abs(df[["error.se1"]])
        df["error5.sp1"] <- abs(df[["error.sp1"]])
        df["error5"] <- rowMeans(df[c("error5.pi", "error5.se1", "error5.sp1")])
        #define extreme parameter space for LMMs
        df["extreme"] <- df$error2.se1 < 0.1 | df$error2.se1 > 0.9 | df$error2.sp1 < 0.1 | df$error2.sp1 > 0.9
        
        # cbind sim, tests, truth, error and then rbind to results
        results <- rbind(results, df)
      }
    }
  }
  # Put results into data frame and rename the rows
  results <- data.frame(results)
  rownames(results) <- 1:nrow(results)
  return (results)
}

#define sepcial melt 2 (for uniform results)

special.melt2 <- function(data) {
  rows <- dim(data)[1]
  # Make a list of new column names for errors
  new.names <- c()
  for (name in dimnames(data)$param){
    new.names <- c(new.names, paste("error" , name, sep="."))
  }
  # Initialise an empty dataframe to put results in
  results <- data.frame(
    array(
      NA, 
      dim=c(0, 2 * dim(data)[5] + 33),
      dimnames=list(sim=c(), param=c("sim", "n.tests", "n.badgers", dimnames(data)$param, new.names, "m.pi", "m.se1", "m.sp1", "error.se", "error.sp", 
                                     "true.se", "true.sp", "error", "sd.se1", "sd.sp1", "sd.pi", "sd.posterior", "precision",
                                     "error2.pi", "error2.se1", "error2.sp1", "error2", "error3.pi", "error3.se1", "error3.sp1", "error3",
                                     "error4.pi", "error4.se1", "error4.sp1", "error4", "error5.pi", "error5.se1", "error5.sp1", "error5",
                                     "extreme"))
    )
  )
  for (t in dimnames(data)$n.tests){
    for (b in dimnames(data)$n.badgers){
      
      # Get array of truth
      truth <- data[, b, t, "true", ]
      std <- data[, b, t, "sd", ]  # Get array of POSTERIOR standard deviations
      m <- data[, b, t, "mean", ]
      
      # Get array of errors and rename columns
      error <- data[, b, t, "error", ]
      dimnames(error)$param <- new.names
      df <- data.frame(cbind(truth, error))
      
      # Add sim and n.tests data
      df["sim"] <- 1:rows
      df["n.tests"] <- as.numeric(t)
      df["n.badgers"] <- as.numeric(b)
      
      # mean
      df["m.pi"] <- as.numeric(m[, "pi"])
      df["m.se1"] <- as.numeric(m[, "se1"])
      df["m.sp1"] <- as.numeric(m[, "sp1"])
      
      df["error.se"] <- rowMeans(error[, grepl("error.se", colnames(error))], na.rm=TRUE)
      df["error.sp"] <- rowMeans(error[, grepl("error.sp", colnames(error))], na.rm=TRUE)
      df["true.se"] <- as.numeric(truth[, "se1"], na.rm=TRUE)
      df["true.sp"] <- as.numeric(truth[, "sp1"], na.rm=TRUE)
      df["error"] <- rowMeans(error[, c("error.pi", "error.se1","error.sp1")], na.rm=TRUE)
      df["sd.se1"] <- std[, "se1"]
      df["sd.sp1"] <- std[, "sp1"]
      df["sd.pi"] <- std[, "pi"]
      df["sd.posterior"] <- rowMeans(std[, c("se1", "sp1", "pi")], na.rm=TRUE)
      df["precision"] <- 1 / df["sd.posterior"]^2
      # Error 2 = logit error no abs
      df["error2.pi"] <- logit.error(truth[, "pi"], m[, "pi"])
      df["error2.se1"] <- logit.error(truth[, "se1"], m[, "se1"])
      df["error2.sp1"] <- logit.error(truth[, "sp1"], m[, "sp1"])
      df["error2"] <- rowMeans(df[c("error2.pi", "error2.se1", "error2.sp1")])
      # Error 3 abs(logit.error)
      df["error3.pi"] <- abs(df[["error2.pi"]]) 
      df["error3.se1"] <- abs(df[["error2.pi"]])
      df["error3.sp1"] <- abs(df[["error2.pi"]])
      df["error3"] <- rowMeans(df[c("error3.pi", "error3.se1", "error3.sp1")])
      # Error 4 = sigma(error3)
      df["error4.pi"] <- inv.logit(df[["error3.pi"]])
      df["error4.se1"] <- inv.logit(df[["error3.se1"]])
      df["error4.sp1"] <- inv.logit(df[["error3.se1"]])
      df["error4"] <- rowMeans(df[c("error4.pi", "error4.se1", "error4.sp1")])
      # Error 5 = absolute error
      df["error5.pi"] <- abs(df[["error.pi"]]) 
      df["error5.se1"] <- abs(df[["error.se1"]])
      df["error5.sp1"] <- abs(df[["error.sp1"]])
      df["error5"] <- rowMeans(df[c("error5.pi", "error5.se1", "error5.sp1")])
      #define extreme parameter space
      df["extreme"] <- df$error2.se1 < 0.1 | df$error2.se1 > 0.9 | df$error2.sp1 < 0.1 | df$error2.sp1 > 0.9
      
      # cbind sim, tests, truth, error and then rbind to results
      results <- rbind(results, df)
    }
  }
  # Put results into data frame and rename the rows
  results <- data.frame(results)
  rownames(results) <- 1:nrow(results)
  return (results)
}

# Load normal results

results <- readRDS(paste(data.directory, "normal-con/results.RData", sep="/"))
results <- special.melt(results)  # Normal

results["constraint"] <- "Constrained"
results["prior.precision"] <- NA
results$prior.precision[which(results$prior.sd == 0.05)] = "Precise"
results$prior.precision[which(results$prior.sd == 0.15)] = "Imprecise"
results["prior.distribution"] <- "Normal"

results2 <- readRDS(paste(data.directory, "normal-uncon/results.RData", sep="/"))
results2 <- special.melt(results2)
results2["constraint"] <- "Unconstrained"
results2["prior.precision"] <- NA
results2$prior.precision[which(results2$prior.sd == 0.05)] = "Precise"
results2$prior.precision[which(results2$prior.sd == 0.15)] = "Imprecise"
results2["prior.distribution"] <- "Normal"

#load uniform results

results3 <- readRDS(paste(data.directory, "uniform-con/results.RData", sep="/"))
results3 <- special.melt2(results3)
results3["constraint"] <- "Constrained"
results3["prior.distribution"] <- "Uniform"
results3["prior.precision"] <- "Uniform"

results4 <- readRDS(paste(data.directory, "uniform-uncon/results.RData", sep="/"))
results4 <- special.melt2(results4)
results4["constraint"] <- "Unconstrained"
results4["prior.distribution"] <- "Uniform"
results4["prior.precision"] <- "Uniform"

# Put results into one dataframe

results <- rbind.fill(results, results2, results3, results4)

# define pi by five categories i.e. put pi results into bins

results$pi2 <- as.numeric(cut(results$pi, breaks=c(0, 0.1, 0.2, 0.3, 0.4, 0.5)))
labels <- c("0 - 0.1", "0.1 - 0.2", "0.2 - 0.3", "0.3 - 0.4", "0.4 - 0.5")
for (i in 1:5) {
  results$pi2[which(results$pi2 == i)] <- labels[i]
}




# ANNA F BUSH
# REGRESSIONS ACROSS PARAMETER SPACE

# load libs

library(lme4)
library(lmerTest)
library(effects)
library(sjPlot)

# all experimental manipulations on normal

#error 2

m1a <- lmer(error2 ~ prior.precision + constraint + n.badgers + n.tests * extreme + 
             (1|se1) + (1|sp1) + (1|pi),
           data = results[which(results$prior.distribution != "Uniform"),])

m1b <- lmer(error2.pi ~ prior.precision + constraint + n.badgers + n.tests * extreme + 
             (1|se1) + (1|sp1) + (1|pi),
           data = results[which(results$prior.distribution != "Uniform"),])

m1c <- lmer(error2.se1 ~ prior.precision + constraint + n.badgers + n.tests * extreme + 
             (1|se1) + (1|sp1) + (1|pi),
           data = results[which(results$prior.distribution != "Uniform"),])

m1d <- lmer(error2.sp1 ~ prior.precision + constraint + n.badgers + n.tests * extreme + 
             (1|se1) + (1|sp1) + (1|pi),
           data = results[which(results$prior.distribution != "Uniform"),])

#error 3

m1e <- lmer(error3 ~ prior.precision + constraint + n.badgers + n.tests * extreme + 
             (1|se1) + (1|sp1) + (1|pi),
           data = results[which(results$prior.distribution != "Uniform"),])

m1f <- lmer(error3.pi ~ prior.precision + constraint + n.badgers + n.tests * extreme + 
             (1|se1) + (1|sp1) + (1|pi),
           data = results[which(results$prior.distribution != "Uniform"),])

m1g <- lmer(error3.se1 ~ prior.precision + constraint + n.badgers + n.tests * extreme + 
             (1|se1) + (1|sp1) + (1|pi),
           data = results[which(results$prior.distribution != "Uniform"),])

m1h <- lmer(error3.sp1 ~ prior.precision + constraint + n.badgers + n.tests * extreme + 
             (1|se1) + (1|sp1) + (1|pi),
           data = results[which(results$prior.distribution != "Uniform"),])


# how different are precise priors to imprecise priors?

#error 2

m2a <- lmer(error2 ~ constraint + n.badgers + n.tests*extreme + 
              (1|se1) + (1|sp1) + (1|pi), data = results[which(results$prior.distribution != "Uniform" & results$prior.precision != "Precise"),])

m2b <- lmer(error2.pi ~ constraint + n.badgers + n.tests*extreme + 
              (1|se1) + (1|sp1) + (1|pi), data = results[which(results$prior.distribution != "Uniform" & results$prior.precision != "Precise"),])

m2c <- lmer(error2.se1 ~ constraint + n.badgers + n.tests*extreme + 
              (1|se1) + (1|sp1) + (1|pi), data = results[which(results$prior.distribution != "Uniform" & results$prior.precision != "Precise"),])

m2d <- lmer(error2.sp1 ~ constraint + n.badgers + n.tests*extreme + 
              (1|se1) + (1|sp1) + (1|pi), data = results[which(results$prior.distribution != "Uniform" & results$prior.precision != "Precise"),])

#error 3

m2e <- lmer(error3 ~ constraint + n.badgers + n.tests*extreme + 
              (1|se1) + (1|sp1) + (1|pi), data = results[which(results$prior.distribution != "Uniform" & results$prior.precision != "Precise"),])

m2f <- lmer(error3.pi ~ constraint + n.badgers + n.tests*extreme + 
              (1|se1) + (1|sp1) + (1|pi), data = results[which(results$prior.distribution != "Uniform" & results$prior.precision != "Precise"),])

m2g <- lmer(error3.se1 ~ constraint + n.badgers + n.tests*extreme + 
              (1|se1) + (1|sp1) + (1|pi), data = results[which(results$prior.distribution != "Uniform" & results$prior.precision != "Precise"),])

m2h <- lmer(error3.sp1 ~ constraint + n.badgers + n.tests*extreme + 
              (1|se1) + (1|sp1) + (1|pi), data = results[which(results$prior.distribution != "Uniform" & results$prior.precision != "Precise"),])


# all expderimental manipulations on uniform

#error 2
m3a <- lmer(error2 ~ constraint + n.badgers + n.tests*extreme + 
             (1|se1) + (1|sp1) + (1|pi), data = results[which(results$prior.distribution != "Normal"),])

m3b <- lmer(error2.pi ~ constraint + n.badgers + n.tests*extreme + 
             (1|se1) + (1|sp1) + (1|pi), data = results[which(results$prior.distribution != "Normal"),])

m3c <- lmer(error2.se1 ~ constraint + n.badgers + n.tests*extreme + 
             (1|se1) + (1|sp1) + (1|pi), data = results[which(results$prior.distribution != "Normal"),])

m3d <- lmer(error2.sp1 ~ constraint + n.badgers + n.tests*extreme + 
             (1|se1) + (1|sp1) + (1|pi), data = results[which(results$prior.distribution != "Normal"),])

#error3

m3e <- lmer(error3 ~ constraint + n.badgers + n.tests*extreme + 
             (1|se1) + (1|sp1) + (1|pi), data = results[which(results$prior.distribution != "Normal"),])

m3f <- lmer(error3.pi ~ constraint + n.badgers + n.tests*extreme + 
             (1|se1) + (1|sp1) + (1|pi), data = results[which(results$prior.distribution != "Normal"),])

m3g <- lmer(error3.se1 ~ constraint + n.badgers + n.tests*extreme + 
             (1|se1) + (1|sp1) + (1|pi), data = results[which(results$prior.distribution != "normal"),])

m3h <- lmer(error3.sp1 ~ constraint + n.badgers + n.tests*extreme + 
             (1|se1) + (1|sp1) + (1|pi), data = results[which(results$prior.distribution != "Normal"),])

# the effect of prior distribution on error

# error 2
m4a <- lmer(error2 ~ prior.distribution + (1|se1) + (1|sp1) + (1|pi), data = results)

m4b <- lmer(error2.pi ~ prior.distribution + (1|se1) + (1|sp1) + (1|pi), data = results)

m4c <- lmer(error2.se1 ~ prior.distribution + (1|se1) + (1|sp1) + (1|pi), data = results)

m4d <- lmer(error2.sp1 ~ prior.distribution + (1|se1) + (1|sp1) + (1|pi), data = results)

# error 3

m4e <- lmer(error3 ~ prior.distribution + (1|se1) + (1|sp1) + (1|pi), data = results)

m4f <- lmer(error3.pi ~ prior.distribution + (1|se1) + (1|sp1) + (1|pi), data = results)

m4g <- lmer(error3.se1 ~ prior.distribution + (1|se1) + (1|sp1) + (1|pi), data = results)

m4h <- lmer(error3.sp1 ~ prior.distribution + (1|se1) + (1|sp1) + (1|pi), data = results)



