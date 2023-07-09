# ANNA BUSH 
# CODE TO MELT RESULTS PRIOR TO HEATMAP and REGRESSION ANALYSES

library(reshape)
library(plyr)
library(Hmisc)
library(boot)
library(base)
library(dplyr)

data.directory <- paste(
  dirname(rstudioapi::getActiveDocumentContext()$path), 
  "../../Results/Simulated dataset 4 - study 1 - constrained space - new truths",
  sep="/"
)
create.dir(working.directory, recursive = TRUE)
setwd(working.directory)


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
      dim=c(0, 2 * dim(data)[6] + 26),
      dimnames=list(sim=c(), param=c("sim", "n.tests", "prior.sd", "n.badgers", dimnames(data)$param, new.names, "error.se", "error.sp", 
                                     "true.se", "true.sp", "error", "sd.se1" , "sd.sp1", "sd.pi", "sd.posterior", 
                                     "error2.pi", "error2.se1", "error2.sp1", "error2", "error3.pi", "error3.se1", "error3.sp1", "error3",
                                     "error4.pi", "error4.se1", "error4.sp1", "error4",
                                     "extreme"))
    )
  )
  for (t in dimnames(data)$n.tests){
    for (p in dimnames(data)$prior.sd){
      for (b in dimnames(data)$n.badgers){
        
        
        truth <- data[, p, b, t, "true", ]  # Get array of truth
        error <- abs(data[, p, b, t, "error", ])  # Get array of errors and rename columns
        std <- data[, p, b, t, "sd", ]  # Get array of POSTERIOR standard deviations
        m <- data[, p, b, t, "mean", ]  #the mean in this context is the prediction
        
        dimnames(error)$param <- new.names
        df <- data.frame(cbind(truth, error))
        
        # Add sim and n.tests data
        df["sim"] <- 1:rows
        df["n.tests"] <- as.numeric(t)
        df["n.badgers"] <- as.numeric(b)
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
      dim=c(0, 2 * dim(data)[5] + 25),
      dimnames=list(sim=c(), param=c("sim", "n.tests", "n.badgers", dimnames(data)$param, new.names, "error.se", "error.sp", 
                                     "true.se", "true.sp", "error", "sd.se1", "sd.sp1", "sd.pi", "sd.posterior",
                                     "error2.pi", "error2.se1", "error2.sp1", "error2", "error3.pi", "error3.se1", "error3.sp1", "error3",
                                     "error4.pi", "error4.se1", "error4.sp1", "error4",
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
      error <- abs(data[, b, t, "error", ])
      dimnames(error)$param <- new.names
      df <- data.frame(cbind(truth, error))
      
      # Add sim and n.tests data
      df["sim"] <- 1:rows
      df["n.tests"] <- as.numeric(t)
      df["n.badgers"] <- as.numeric(b)
      df["error.se"] <- rowMeans(error[, grepl("error.se", colnames(error))], na.rm=TRUE)
      df["error.sp"] <- rowMeans(error[, grepl("error.sp", colnames(error))], na.rm=TRUE)
      df["true.se"] <- as.numeric(truth[, "se1"], na.rm=TRUE)
      df["true.sp"] <- as.numeric(truth[, "sp1"], na.rm=TRUE)
      df["error"] <- rowMeans(error[, c("error.pi", "error.se1","error.sp1")], na.rm=TRUE)
      df["sd.se1"] <- std[, "se1"]
      df["sd.sp1"] <- std[, "sp1"]
      df["sd.pi"] <- std[, "pi"]
      df["sd.posterior"] <- rowMeans(std[, c("se1", "sp1", "pi")], na.rm=TRUE)
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
results1 <- readRDS(paste(data.dir, "/normal_con/results.RData", sep="/"))
results1 <- special.melt(results1)
results1["prior.precision"] <- NA
results1["constraint"] <- "Constrained"
results1$prior.precision[which(results1$prior.sd == 0.05)] = "Precise"
results1$prior.precision[which(results1$prior.sd == 0.15)] = "Imprecise"
results1["prior.distribution"] <- "Normal"

results2 <- readRDS(paste(data.dir, "/normal_uncon/results.RData", sep="/"))
results2 <- special.melt(results2)
results2["prior.precision"] <- NA
results2["constraint"] <- "Unconstrained"
results2$prior.precision[which(results2$prior.sd == 0.05)] = "Precise"
results2$prior.precision[which(results2$prior.sd == 0.15)] = "Imprecise"
results2["prior.distribution"] <- "Normal"

results3 <-  readRDS(paste(data.dir, "/uniform_con/results.RData", sep="/"))
results3 <- special.melt2(results3)
results3["prior.precision"] <- "Uniform"
results3["constraint"] <- "Constrained"
results3["prior.distribution"] <- "Uniform"

results4 <-  readRDS(paste(data.dir, "/uniform_uncon/results.RData", sep="/"))
results4 <- special.melt2(results4)
results4["prior.precision"] <- "Uniform"
results4["constraint"] <- "Unconstrained"
results4["prior.distribution"] <- "Uniform"

# Put results into one dataframe
results <- rbind.fill(results1, results2, results3, results4)

# Define pi by five categories i.e. put pi results into bins
results$pi2 <- as.numeric(cut(results$pi, breaks=c(0, 0.1, 0.2, 0.3, 0.4, 0.5)))
labels <- c("0 - 0.1", "0.1 - 0.2", "0.2 - 0.3", "0.3 - 0.4", "0.4 - 0.5")
for (i in 1:5) {
  results$pi2[which(results$pi2 == i)] <- labels[i]
}

