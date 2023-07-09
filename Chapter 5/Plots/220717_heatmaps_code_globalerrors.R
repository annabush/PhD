# ANNA BUSH 
# CODE TO MELT RESULTS PRIOR TO HEATMAP and REGRESSION ANALYSES

library(reshape)
library(plyr)
library(Hmisc)
library(boot)
library(base)
library(dplyr)
library(stringr)

working.directory <- paste(dirname(rstudioapi::getActiveDocumentContext()$path), "global errors and precisions", sep="/")
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



# ANNA F BUSH
# CODE FOR 2D AND 3D HEATMAPS

# load libs

library(ggplot2)
library(rayshader)
library(magick)
library(viridis)

# set colour of heatmaps
nonabs <- c("blue", "white", "red")
#abso <- c("white", "seashell", "pink", "red")
abso <- c("green", "pink", "red")
#prec <- c("white", "honeydew", "green", "darkgreen")
prec <- c("green", "pink", "red")
#three_d <- rainbow(5)
#colours <- three_d

# 18 2D ggplot heatmaps of error across parameter space facetted by:

# PRIOR PRECISION ---------------------------------------------------------

## plotting accuracy

#non absolute
plot1 <- ggplot(results[which(results$prior.distribution != "Uniform"),], aes(se1, sp1, z=error)) +
  geom_tile(colour = "white", binwidth=0.5/5, stat = "summary2d")+
  coord_fixed()+
  scale_y_continuous(breaks = seq(0.5, 1, 0.5))+
  scale_x_continuous(breaks = seq(0, 1, 0.5))+
  scale_y_continuous(sec.axis = sec_axis(~ ., name = "True Pi", breaks=NULL))+
  xlab("True Se")+
  ylab("True Sp")+
  facet_grid(pi2 ~ prior.precision)+
  scale_fill_gradientn(colours = nonabs, limits=c(-0.06, 0.06), name = "Inferential Error", labels = function(x) sprintf("%.2f", x))+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank())+
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))
ggsave(filename="plot_01.png", plot=plot1, width=5.9)

#absolute
plot2 <- ggplot(results[which(results$prior.distribution != "Uniform"),], aes(se1, sp1, z=error5)) +
  geom_tile(colour = "white", binwidth=0.5/5, stat = "summary2d")+
  coord_fixed()+
  scale_y_continuous(breaks = seq(0.5, 1, 0.5))+
  scale_x_continuous(breaks = seq(0, 1, 0.5))+
  scale_y_continuous(sec.axis = sec_axis(~ ., name = "True Pi", breaks=NULL))+
  xlab("True Se")+
  ylab("True Sp")+
  facet_grid(pi2 ~ prior.precision)+
  scale_fill_gradientn(colours = abso, limits=c(0, 0.06), name = "Absolute Inferential \nError", labels = function(x) sprintf("%.2f", x))+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank())+
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))
ggsave(filename="plot_02.png", plot=plot2, width=5.9)


## plotting precision

plot3 <- ggplot(results[which(results$prior.distribution != "Uniform"),], aes(se1, sp1, z=sd.posterior)) +
  geom_tile(colour = "white", binwidth=0.5/5, stat = "summary2d")+
  coord_fixed()+
  scale_y_continuous(breaks = seq(0.5, 1, 0.5))+
  scale_x_continuous(breaks = seq(0, 1, 0.5))+ 
  scale_y_continuous(sec.axis = sec_axis(~ ., name = "True Pi", breaks=NULL))+
  xlab("True Se")+
  ylab("True Sp")+
  facet_grid(pi2 ~ prior.precision)+
  scale_fill_gradientn(colours = prec, name = "Standard Deviation", labels = function(x) sprintf("%.2f", x))+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank())+
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))
ggsave(filename="plot_03.png", plot=plot3, width=5.9)


# constraint - normal -----------------------------------------------------
#plotting accuracy
#not absolute
plot4 <- ggplot(results[which(results$prior.distribution != "Uniform"),], aes(se1, sp1, z=error) ) +
  geom_tile(colour = "white", binwidth=0.5/5, stat = "summary2d")+
  coord_fixed()+
  scale_y_continuous(breaks = seq(0.5, 1, 0.1))+
  scale_x_continuous(breaks = seq(0, 1, 0.5))+
  scale_y_continuous(sec.axis = sec_axis(~ ., name = "True Pi", breaks=NULL))+
  xlab("True Se")+
  ylab("True Sp")+
  facet_grid(pi2 ~ constraint)+
  scale_fill_gradientn(colours = nonabs, limits=c(-0.04, 0.04), name = "Inferential Error", labels = function(x) sprintf("%.2f", x))+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank())+
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))
ggsave(filename="plot_04.png", plot=plot4, width=5.9)

#absolute
plot5 <- ggplot(results[which(results$prior.distribution != "Uniform"),], aes(se1, sp1, z=error5) ) +
  geom_tile(colour = "white", binwidth=0.5/5, stat = "summary2d")+
  coord_fixed()+
  scale_y_continuous(breaks = seq(0.5, 1, 0.1))+
  scale_x_continuous(breaks = seq(0, 1, 0.5))+
  #scale_y_continuous(sec.axis = sec_axis(~ ., name = "True Pi", breaks=NULL))+
  xlab("True Se")+
  ylab("True Sp")+
  facet_grid(pi2 ~ constraint)+
  scale_fill_gradientn(colours = abso, limits=c(0, 0.05), name = "Absolute Inferential \nError", labels = function(x) sprintf("%.2f", x))+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank())+
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))
ggsave(filename="plot_05.png", plot=plot5, width=5.9)

#plotting precision

plot6 <- ggplot(results[which(results$prior.distribution != "Uniform"),], aes(se1, sp1, z=sd.posterior) ) +
  geom_tile(colour = "white", binwidth=0.5/5, stat = "summary2d")+
  coord_fixed()+
  scale_y_continuous(breaks = seq(0.5, 1, 0.1))+
  scale_x_continuous(breaks = seq(0, 1, 0.5))+
  scale_y_continuous(sec.axis = sec_axis(~ ., name = "True Pi", breaks=NULL))+
  xlab("True Se")+
  ylab("True Sp")+
  facet_grid(pi2 ~ constraint)+
  scale_fill_gradientn(colours = prec, name = "Standard Deviation", labels = function(x) sprintf("%.2f", x))+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank())+
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))
ggsave(filename="plot_06.png", plot=plot6, width=5.9)

# constraint - uniform ----------------------------------------------------

#plotting accuracy
#not absolute
plot7 <- ggplot(results[which(results$prior.distribution != "Normal"),], aes(se1, sp1, z=error) ) +
  geom_tile(colour = "white", binwidth=0.5/5, stat = "summary2d")+
  coord_fixed()+
  scale_y_continuous(breaks = seq(0.5, 1, 0.1))+
  scale_x_continuous(breaks = seq(0, 1, 0.5))+
  scale_y_continuous(sec.axis = sec_axis(~ ., name = "True Pi", breaks=NULL))+
  xlab("True Se")+
  ylab("True Sp")+
  facet_grid(pi2 ~ constraint)+
  scale_fill_gradientn(colours = nonabs, limits=c(-0.4, 0.4), name = "Inferential Error", labels = function(x) sprintf("%.2f", x))+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank())+
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))
ggsave(filename="plot_07.png", plot=plot7, width=5.9)

#absolute
plot8 <- ggplot(results[which(results$prior.distribution != "Normal"),], aes(se1, sp1, z=error5) ) +
  geom_tile(colour = "white", binwidth=0.5/5, stat = "summary2d")+
  coord_fixed()+
  scale_y_continuous(breaks = seq(0.5, 1, 0.1))+
  scale_x_continuous(breaks = seq(0, 1, 0.5))+
  scale_y_continuous(sec.axis = sec_axis(~ ., name = "True Pi", breaks=NULL))+
  xlab("True Se")+
  ylab("True Sp")+
  facet_grid(pi2 ~ constraint)+
  scale_fill_gradientn(colours = abso, name = "Absolute Inferential \nError", labels = function(x) sprintf("%.2f", x))+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank())+
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))
ggsave(filename="plot_08.png", plot=plot8, width=5.9)

#plotting precision

plot9 <- ggplot(results[which(results$prior.distribution != "Normal"),], aes(se1, sp1, z=sd.posterior) ) +
  geom_tile(colour = "white", binwidth=0.5/5, stat = "summary2d")+
  coord_fixed()+
  scale_y_continuous(breaks = seq(0.5, 1, 0.1))+
  scale_x_continuous(breaks = seq(0, 1, 0.5))+
  scale_y_continuous(sec.axis = sec_axis(~ ., name = "True Pi", breaks=NULL))+
  xlab("True Se")+
  ylab("True Sp")+
  facet_grid(pi2 ~ constraint)+
  scale_fill_gradientn(colours = prec, name = "Standard Deviation", labels = function(x) sprintf("%.2f", x))+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank())+
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))
ggsave(filename="plot_09.png", plot=plot9, width=5.9)

# n.samples ---------------------------------------------------------------

# plotting accuracy

plot10 <- ggplot(results[which(results$prior.distribution != "Uniform"),], aes(se1, sp1, z=error) ) +
  geom_tile(colour = "white", binwidth=0.5/5, stat = "summary2d")+
  coord_fixed()+
  scale_y_continuous(breaks = seq(0.5, 1, 0.1))+
  scale_x_continuous(breaks = seq(0, 1, 0.5))+
  scale_y_continuous(sec.axis = sec_axis(~ ., name = "True Pi", breaks=NULL))+
  xlab("True Se")+
  ylab("True Sp")+
  facet_grid(pi2 ~ n.badgers)+
  scale_fill_gradientn(colours = nonabs, limits=c(-0.04, 0.04), name = "Inferential Error", labels = function(x) sprintf("%.2f", x))+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank())+
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))
ggsave(filename="plot_10.png", plot=plot10, width=5.9)

plot11 <- ggplot(results[which(results$prior.distribution != "Uniform"),], aes(se1, sp1, z=error5) ) +
  geom_tile(colour = "white", binwidth=0.5/5, stat = "summary2d")+
  coord_fixed()+
  scale_y_continuous(breaks = seq(0.5, 1, 0.1))+
  scale_x_continuous(breaks = seq(0, 1, 0.5))+
  scale_y_continuous(sec.axis = sec_axis(~ ., name = "True Pi", breaks=NULL))+
  xlab("True Se")+
  ylab("True Sp")+
  facet_grid(pi2 ~ n.badgers)+
  scale_fill_gradientn(colours = abso, limits=c(0, 0.05), name = "Absolute Inferential \nError", labels = function(x) sprintf("%.2f", x))+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank())+
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))
ggsave(filename="plot_11.png", plot=plot11, width=5.9)

#plotting precision

plot12 <- ggplot(results[which(results$prior.distribution != "Uniform"),], aes(se1, sp1, z=sd.posterior) ) +
  geom_tile(colour = "white", binwidth=0.5/5, stat = "summary2d")+
  coord_fixed()+
  scale_y_continuous(breaks = seq(0.5, 1, 0.1))+
  scale_x_continuous(breaks = seq(0, 1, 0.5))+
  scale_y_continuous(sec.axis = sec_axis(~ ., name = "True Pi", breaks=NULL))+
  xlab("True Se")+
  ylab("True Sp")+
  facet_grid(pi2 ~ n.badgers)+
  scale_fill_gradientn(colours = prec, name = "Standard Deviation", labels = function(x) sprintf("%.2f", x))+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank())+
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))
ggsave(filename="plot_12.png", plot=plot12, width=5.9)

# n.tests -----------------------------------------------------------------

#plotting accuracy
#not absoluted
plot13 <- ggplot(results[which(results$prior.distribution != "Uniform"),], aes(se1, sp1, z=error) ) +
  geom_tile(colour = "white", binwidth=0.5/5, stat = "summary2d")+
  coord_fixed()+
  scale_y_continuous(breaks = seq(0.5, 1, 0.1))+
  scale_x_continuous(breaks = seq(0, 1, 0.5))+
  scale_y_continuous(sec.axis = sec_axis(~ ., name = "True Pi", breaks=NULL))+
  xlab("True Se")+
  ylab("True Sp")+
  facet_grid(pi2 ~ n.tests)+
  scale_fill_gradientn(colours = nonabs, limits=c(-0.09, 0.09), name = "Inferential Error", labels = function(x) sprintf("%.2f", x))+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank())+
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))
ggsave(filename="plot_13.png", plot=plot13,width=277, height = 150, units = "mm") #landscape

#absolute
plot14 <- ggplot(results[which(results$prior.distribution != "Uniform"),], aes(se1, sp1, z=error5) ) +
  geom_tile(colour = "white", binwidth=0.5/5, stat = "summary2d")+
  coord_fixed()+
  scale_y_continuous(breaks = seq(0.5, 1, 0.1))+
  scale_x_continuous(breaks = seq(0, 1, 0.5))+
  scale_y_continuous(sec.axis = sec_axis(~ ., name = "True Pi", breaks=NULL))+
  xlab("True Se")+
  ylab("True Sp")+
  facet_grid(pi2 ~ n.tests)+
  scale_fill_gradientn(colours = abso, limits=c(0, 0.1), name = "Absolute Inferential \nError", labels = function(x) sprintf("%.2f", x))+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank())+
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))
ggsave(filename="plot_14.png", plot=plot14, width=277, height = 150, units = "mm") #landscape

#plotting precision

plot15 <- ggplot(results[which(results$prior.distribution != "Uniform"),], aes(se1, sp1, z=sd.posterior) ) +
  geom_tile(colour = "white", binwidth=0.5/5, stat = "summary2d")+
  coord_fixed()+
  scale_y_continuous(breaks = seq(0.5, 1, 0.1))+
  scale_x_continuous(breaks = seq(0, 1, 0.5))+
  scale_y_continuous(sec.axis = sec_axis(~ ., name = "True Pi", breaks=NULL))+
  xlab("True Se")+
  ylab("True Sp")+
  facet_grid(pi2 ~ n.tests)+
  scale_fill_gradientn(colours = prec, name = "Standard Deviation", labels = function(x) sprintf("%.2f", x))+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank())+
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))
ggsave(filename="plot_15.png", plot=plot15, width=277, height = 150, units = "mm") #landscape

# prior distribution (normal v uniform) -----------------------------------

#plotting accuracy

plot16 <- ggplot(results[which(results$prior.precision != "Precise"),], aes(se1, sp1, z=error) ) +
  geom_tile(colour = "white", binwidth=0.5/5, stat = "summary2d")+
  coord_fixed()+
  scale_y_continuous(breaks = seq(0.5, 1, 0.1))+
  scale_x_continuous(breaks = seq(0, 1, 0.5))+
  scale_y_continuous(sec.axis = sec_axis(~ ., name = "True Pi", breaks=NULL))+
  xlab("True Se")+
  ylab("True Sp")+
  facet_grid(pi2 ~ prior.distribution)+
  scale_fill_gradientn(colours = nonabs, limits=c(-0.23, 0.23), name = "Inferential Error", labels = function(x) sprintf("%.2f", x))+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank())+
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))
ggsave(filename="plot_16.png", plot=plot16, width=5.9)


plot17 <- ggplot(results[which(results$prior.precision != "Precise"),], aes(se1, sp1, z=error5) ) +
  geom_tile(colour = "white", binwidth=0.5/5, stat = "summary2d")+
  coord_fixed()+
  scale_y_continuous(breaks = seq(0.5, 1, 0.1))+
  scale_x_continuous(breaks = seq(0, 1, 0.5))+
  scale_y_continuous(sec.axis = sec_axis(~ ., name = "True Pi", breaks=NULL))+
  xlab("True Se")+
  ylab("True Sp")+
  facet_grid(pi2 ~ prior.distribution)+
  scale_fill_gradientn(colours = abso, limits=c(0, 0.31), name = "Absolute Inferential \nError", labels = function(x) sprintf("%.2f", x))+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank())+
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))
ggsave(filename="plot_17.png", plot=plot17, width=5.9)

#plotting precision

plot18 <- ggplot(results[which(results$prior.precision != "Precise"),], aes(se1, sp1, z=sd.posterior) ) +
  geom_tile(colour = "white", binwidth=0.5/5, stat = "summary2d")+
  coord_fixed()+
  scale_y_continuous(breaks = seq(0.5, 1, 0.1))+
  scale_x_continuous(breaks = seq(0, 1, 0.5))+
  scale_y_continuous(sec.axis = sec_axis(~ ., name = "True Pi", breaks=NULL))+
  xlab("True Se")+
  ylab("True Sp")+
  facet_grid(pi2 ~ prior.distribution)+
  scale_fill_gradientn(colours = prec, name = "Standard Deviation", labels = function(x) sprintf("%.2f", x))+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank())+
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))
ggsave(filename="plot_18.png", plot=plot18, width=5.9)


# code to plot 3D versions of above ggplots

# first, set colours to three_d and re-run the above sections
# insert the correct plot into the plot_gg function below

#plot_gg(plotA)

