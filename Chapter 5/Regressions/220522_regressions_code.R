# ANNA BUSH
# REGRESSIONS ACROSS PARAMETER SPACE

library(reshape)
library(plyr)
library(Hmisc)
library(boot)
library(base)
library(dplyr)
library(stringr)
library(lme4)
library(lmerTest)
library(effects)
library(sjPlot)
library(stargazer)

working.directory <- paste(dirname(rstudioapi::getActiveDocumentContext()$path), "tables", sep="/")
create.dir(working.directory, recursive = TRUE)
setwd(working.directory)

# Define logit error
logit.error <- function(truth, pred) {
  return(log(pred/(1 - pred)) - log(truth / (1 - truth)))
}

#define special melt (for normal results)

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
      dim=c(0, 2 * dim(data)[6] + 31),
      dimnames=list(sim=c(), param=c("sim", "n.tests", "prior.sd", "n.badgers", dimnames(data)$param, new.names, "error.se", "error.sp", 
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
      dim=c(0, 2 * dim(data)[5] + 30),
      dimnames=list(sim=c(), param=c("sim", "n.tests", "n.badgers", dimnames(data)$param, new.names, "error.se", "error.sp", 
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

#load normal results

results <- readRDS("C:/Users/anna-/OneDrive - University of Exeter/ANNA PhD WRITE-UP/6. Artefacts - SeSp tradeoff/Results/normal-con/results.RData")
results <- special.melt(results)  # Normal

results["constraint"] <- "Constrained"
results["prior.precision"] <- NA
results$prior.precision[which(results$prior.sd == 0.05)] = "Precise"
results$prior.precision[which(results$prior.sd == 0.15)] = "Imprecise"
results["prior.distribution"] <- "Normal"

results2 <- readRDS("C:/Users/anna-/OneDrive - University of Exeter/ANNA PhD WRITE-UP/6. Artefacts - SeSp tradeoff/Results/normal-uncon/results.RData")
results2 <- special.melt(results2)
results2["constraint"] <- "Unconstrained"
results2["prior.precision"] <- NA
results2$prior.precision[which(results2$prior.sd == 0.05)] = "Precise"
results2$prior.precision[which(results2$prior.sd == 0.15)] = "Imprecise"
results2["prior.distribution"] <- "Normal"

#load uniform results

results3 <- readRDS("C:/Users/anna-/OneDrive - University of Exeter/ANNA PhD WRITE-UP/6. Artefacts - SeSp tradeoff/Results/uniform-con/results.RData")
results3 <- special.melt2(results3)
results3["constraint"] <- "Constrained"
results3["prior.distribution"] <- "Uniform"
results3["prior.precision"] <- "Uniform"

results4 <- readRDS("C:/Users/anna-/OneDrive - University of Exeter/ANNA PhD WRITE-UP/6. Artefacts - SeSp tradeoff/Results/uniform-uncon/results.RData")
results4 <- special.melt2(results4)
results4["constraint"] <- "Unconstrained"
results4["prior.distribution"] <- "Uniform"
results4["prior.precision"] <- "Uniform"

#put results into one dataframe

results <- rbind.fill(results, results2, results3, results4)

# define pi by five categories i.e. put pi results into bins

results$pi2 <- as.numeric(cut(results$pi, breaks=c(0, 0.1, 0.2, 0.3, 0.4, 0.5)))
labels <- c("0 - 0.1", "0.1 - 0.2", "0.2 - 0.3", "0.3 - 0.4", "0.4 - 0.5")
for (i in 1:5) {
  results$pi2[which(results$pi2 == i)] <- labels[i]
}


#### DO REGRESSIONS ######

# all experimental manipulations on normal

#non-absolute errors
#LMM1
LMM1 <- lmer(error ~ prior.precision + constraint + n.badgers + n.tests* extreme + 
             (1|se1) + (1|sp1) + (1|pi),
           data = results[which(results$prior.distribution != "Uniform"),])

#absolute errors
#LMM2
LMM2 <- lmer(error5 ~ prior.precision + constraint + n.badgers + n.tests* extreme + 
              (1|se1) + (1|sp1) + (1|pi),
            data = results[which(results$prior.distribution != "Uniform"),])

#standard deviation
#LMM3
LMM3 <- lmer(sd.posterior ~ prior.precision + constraint + n.badgers + n.tests*extreme + 
              (1|se1) + (1|sp1) + (1|pi), data = results[which(results$prior.distribution != "Uniform"),])


# how different are precise priors to imprecise priors?
#what happens when imprecise priors are given?

#non-absolute errors
#LMM4
LMM4 <- lmer(error ~ constraint + n.badgers + n.tests*extreme + 
              (1|se1) + (1|sp1) + (1|pi), data = results[which(results$prior.distribution != "Uniform" & results$prior.precision != "Precise"),])

#absolute errors
#LMM5
LMM5 <- lmer(error5 ~ constraint + n.badgers + n.tests*extreme + 
              (1|se1) + (1|sp1) + (1|pi), data = results[which(results$prior.distribution != "Uniform" & results$prior.precision != "Precise"),])

#standard deviation
#LMM6
LMM6 <- lmer(sd.posterior ~ constraint + n.badgers + n.tests*extreme + 
              (1|se1) + (1|sp1) + (1|pi), data = results[which(results$prior.distribution != "Uniform" & results$prior.precision != "Precise"),])


# all expderimental manipulations on uniform

#non-absolute
#LMM7
LMM7 <- lmer(error ~ constraint + n.badgers + n.tests*extreme + 
             (1|se1) + (1|sp1) + (1|pi), data = results[which(results$prior.distribution != "Normal"),])

#absolute
#LMM8
LMM8 <- lmer(error5 ~ constraint + n.badgers + n.tests*extreme + 
             (1|se1) + (1|sp1) + (1|pi), data = results[which(results$prior.distribution != "Normal"),])

#standard deviation
#LMM9
LMM9 <- lmer(sd.posterior ~ constraint + n.badgers + n.tests*extreme + 
              (1|se1) + (1|sp1) + (1|pi), data = results[which(results$prior.distribution != "Normal"),])

# the effect of prior distribution on error
#non-absolute
#LMM10
LMM10 <- lmer(error ~ prior.distribution + (1|se1) + (1|sp1) + (1|pi), data = results)

#absolute
#LMM11
LMM11 <- lmer(error5 ~ prior.distribution + (1|se1) + (1|sp1) + (1|pi), data = results)

#standard deviation
#LMM12
LMM12 <- lmer(sd.posterior ~ prior.distribution + (1|se1) + (1|sp1) + (1|pi), data = results)


### regressions specifically for the 15% problem ######

# all experimental manipulations on normal

data <- results[which(results$prior.distribution != "Uniform"),]
data2 <- data[data$pi %in% c("0.15"),]

#non-absolute errors
#LMM13
LMM13 <- lmer(error ~ prior.precision + constraint + n.badgers + n.tests + 
              (1|se1) + (1|sp1),
            data = data2)

#absolute errors
#LMM14
LMM14 <- lmer(error5 ~ prior.precision + constraint + n.badgers + n.tests + 
              (1|se1) + (1|sp1),
            data = data2)

#standard deviation
#LMM15
LMM15 <- lmer(sd.posterior ~ prior.precision + constraint + n.badgers + n.tests + 
              (1|se1) + (1|sp1), data = data2)


# when only imprecise priors used?

data3 <- data2[which(data2$prior.precision != "Precise"),]

#non-absolute errors
#LMM16
LMM16 <- lmer(error ~ constraint + n.badgers + n.tests + 
              (1|se1) + (1|sp1), data = data3)

#absolute errors
#LMM17
LMM17 <- lmer(error5 ~ constraint + n.badgers + n.tests + 
              (1|se1) + (1|sp1), data = data3)

#LMM18
#standard deviation
LMM18 <- lmer(sd.posterior ~ constraint + n.badgers + n.tests + 
              (1|se1) + (1|sp1), data = data3)


# all expderimental manipulations on uniform

data4 <- results[which(results$prior.distribution != "Normal"),]
data5 <- data[data$pi %in% c("0.15"),]

#non-absolute
#LMM19
LMM19 <- lmer(error ~ constraint + n.badgers + n.tests + 
              (1|se1) + (1|sp1), data = data5)

#absolute
#LMM20
LMM20 <- lmer(error5 ~ constraint + n.badgers + n.tests + 
              (1|se1) + (1|sp1), data = data5)

#standard deviation
#LMM21
LMM21 <- lmer(sd.posterior ~ constraint + n.badgers + n.tests + 
              (1|se1) + (1|sp1), data = data5)

# the effect of prior distribution on error
data6 <- results[results$pi %in% c("0.15"),]

#non-absolute
#LMM22
LMM22 <- lmer(error ~ prior.distribution + (1|se1) + (1|sp1), data = data6)


#absolute
#LMM23
LMM23 <- lmer(error5 ~ prior.distribution + (1|se1) + (1|sp1), data = data6)


#standard deviation
#LMM24
LMM24 <- lmer(sd.posterior ~ prior.distribution + (1|se1) + (1|sp1), data = data6)

#### regressions on specific parameter errors
#### first SE ###

#non-absolute errors
#LMM25
LMM25 <- lmer(error.se1 ~ prior.precision + constraint + n.badgers + n.tests* extreme + 
              (1|se1) + (1|sp1) + (1|pi),
            data = results[which(results$prior.distribution != "Uniform"),])

#absolute errors
#LMM26
LMM26 <- lmer(error5.se1 ~ prior.precision + constraint + n.badgers + n.tests* extreme + 
              (1|se1) + (1|sp1) + (1|pi),
            data = results[which(results$prior.distribution != "Uniform"),])

#standard deviation
#LMM27
LMM27 <- lmer(sd.se1 ~ prior.precision + constraint + n.badgers + n.tests*extreme + 
              (1|se1) + (1|sp1) + (1|pi), data = results[which(results$prior.distribution != "Uniform"),])

#### SP #####

#non-absolute errors
#LMM28
LMM28 <- lmer(error.sp1 ~ prior.precision + constraint + n.badgers + n.tests* extreme + 
              (1|se1) + (1|sp1) + (1|pi),
            data = results[which(results$prior.distribution != "Uniform"),])

#absolute errors
#LMM29
LMM29 <- lmer(error5.sp1 ~ prior.precision + constraint + n.badgers + n.tests* extreme + 
              (1|se1) + (1|sp1) + (1|pi),
            data = results[which(results$prior.distribution != "Uniform"),])

#standard deviation
#LMM30
LMM30 <- lmer(sd.sp1 ~ prior.precision + constraint + n.badgers + n.tests*extreme + 
              (1|se1) + (1|sp1) + (1|pi), data = results[which(results$prior.distribution != "Uniform"),])

######## PI #########

#non-absolute errors
#LMM31
LMM31 <- lmer(error.pi ~ prior.precision + constraint + n.badgers + n.tests* extreme + 
              (1|se1) + (1|sp1) + (1|pi),
            data = results[which(results$prior.distribution != "Uniform"),])

#absolute errors
#LMM32
LMM32 <- lmer(error5.pi ~ prior.precision + constraint + n.badgers + n.tests* extreme + 
              (1|se1) + (1|sp1) + (1|pi),
            data = results[which(results$prior.distribution != "Uniform"),])

#standard deviation
#LMM33
LMM33 <- lmer(sd.pi ~ prior.precision + constraint + n.badgers + n.tests*extreme + 
              (1|se1) + (1|sp1) + (1|pi), data = results[which(results$prior.distribution != "Uniform"),])


#### parameter specific errors for the 15% problem.

data <- results[which(results$prior.distribution != "Uniform"),]
data2 <- data[data$pi %in% c("0.15"),]

####SE#####

#non-absolute errors
#LMM34
LMM34 <- lmer(error.se1 ~ prior.precision + constraint + n.badgers + n.tests + 
                (1|se1) + (1|sp1),
              data = data2)

#absolute errors
#LMM35
LMM35 <- lmer(error5.se1 ~ prior.precision + constraint + n.badgers + n.tests + 
                (1|se1) + (1|sp1),
              data = data2)

#standard deviation
#LMM36
LMM36 <- lmer(sd.se1 ~ prior.precision + constraint + n.badgers + n.tests + 
                (1|se1) + (1|sp1), data = data2)

####Sp#####

#non-absolute errors
#LMM37
LMM37 <- lmer(error.sp1 ~ prior.precision + constraint + n.badgers + n.tests + 
                (1|se1) + (1|sp1),
              data = data2)

#absolute errors
#LMM38
LMM38 <- lmer(error5.sp1 ~ prior.precision + constraint + n.badgers + n.tests + 
                (1|se1) + (1|sp1),
              data = data2)

#standard deviation
#LMM39
LMM39 <- lmer(sd.sp1 ~ prior.precision + constraint + n.badgers + n.tests + 
                (1|se1) + (1|sp1), data = data2)

####PI#####

#non-absolute errors
#LMM40
LMM40 <- lmer(error.pi ~ prior.precision + constraint + n.badgers + n.tests + 
                (1|se1) + (1|sp1),
              data = data2)

#absolute errors
#LMM41
LMM41 <- lmer(error5.pi ~ prior.precision + constraint + n.badgers + n.tests + 
                (1|se1) + (1|sp1),
              data = data2)

#standard deviation
#LMM42
LMM42 <- lmer(sd.pi ~ prior.precision + constraint + n.badgers + n.tests + 
                (1|se1) + (1|sp1), data = data2)
