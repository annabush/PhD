"
Anna Bush

Script to create multi-panel plots showing error changes with truthspace
the original plot contained within case-study-1 is expanded across 
truthsapce in 6 ways:
  pi increase
  pi decrease
  se increase
  se decrease
  sp increase
  sp decrease
"

library(cowplot)
library(lme4)
library(lmerTest)
library(reshape2)
library(ggplot2)
library(arm)
library(leaps)

working.directory <- dirname(rstudioapi::getActiveDocumentContext()$path)
data.directory <- "../Simulated datasets 1 and 2/study 1"

orig.path <- "Original/results.RData"
r1.path <- "P increase/results.RData"
r2.path <- "P decrease/results.RData"
r3.path <- "Se increase/results.RData"
r4.path <- "Se decrease/results.RData"
r5.path <- "Sp increase/results.RData"
r6.path <- "Sp decrease/results.RData"

setwd(working.directory)
  
original <- readRDS(paste(data.directory, orig.path, sep="/"))
original <- melt(original)

r1 <- readRDS(paste(data.directory, r1.path, sep="/"))
r1 <- melt(r1)
r1["status"] <- "increase"
r1["parameter"] <- "pi"

r2 <- readRDS(paste(data.directory, r2.path, sep="/"))
r2 <- melt(r2)
r2["status"] <- "decrease"
r2["parameter"] <- "pi"

r3 <- readRDS(paste(data.directory, r3.path, sep="/"))
r3 <- melt(r3)
r3["status"] <- "increase"
r3["parameter"] <- "se"

r4 <- readRDS(paste(data.directory, r4.path, sep="/"))
r4 <- melt(r4)
r4["status"] <- "decrease"
r4["parameter"] <- "se"

r5 <- readRDS(paste(data.directory, r5.path, sep="/"))
r5 <- melt(r5)
r5["status"] <- "increase"
r5["parameter"] <- "sp"

r6 <- readRDS(paste(data.directory, r6.path, sep="/"))
r6 <- melt(r6)
r6["status"] <- "decrease"
r6["parameter"] <- "sp"

results <- data.frame(rbind(r1, r2, r3, r4, r5, r6))

df <- results[which(results$statistic == "error" & !is.na(match(results$param, c("se1", "sp1", "pi")))),]
df$value = abs(df$value)
ggplot(df, aes(x=n.tests, y=value, group=param, color=param)) + theme_bw()+
  stat_summary(fun.y=mean, geom="line", size = 0.8, position=position_dodge(width=0.25)) +
  stat_summary(fun.y=mean, geom="point", size=2, position=position_dodge(width=0.25)) +
  stat_summary(
    fun.y=mean,
    fun.ymin=function(x) mean(x) - sd(x), 
    fun.ymax=function(x) mean(x) + sd(x), 
    width=0.1,
    size=0.8,
    geom="errorbar",
    position=position_dodge(width=0.25)
  ) +
  xlab("Number of diagnostic tests") + 
  ylab("Absolute error") + 
  labs(col="Parameter")+
  facet_grid(parameter ~ status)

