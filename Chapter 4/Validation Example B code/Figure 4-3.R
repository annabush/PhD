"
Anna Bush

Plot the results of cast study 2
"

library(reshape2)
library(ggplot2)

working.directory <- dirname(rstudioapi::getActiveDocumentContext()$path)
data.directory <- "../Simulated datasets 1 and 2/study 2"


con.sp.path <- "constrained sp/results.RData"
con.pi.path <- "constrained pi/results.RData"
con.sp.pi.path <- "constrained sppi/results.RData"
uncon.path <- "unconstrained/results.RData"

setwd(working.directory)

# Get data
results_con_sp <- readRDS(paste(data.directory, con.sp.path, sep="/"))  
results_con_sp <- melt(results_con_sp)
results_con_sp["constraint"] <- "constrained Sp"

results_con_pi <- readRDS(paste(data.directory, con.pi.path, sep="/"))
results_con_pi <- melt(results_con_pi)
results_con_pi["constraint"] <- "constrained Pi"

results_con_sppi <- readRDS(paste(data.directory, con.sp.pi.path, sep="/"))
results_con_sppi <- melt(results_con_sppi)
results_con_sppi["constraint"] <- "constrained Sp Pi"

results_uncon <- readRDS(paste(data.directory, uncon.path, sep="/"))
results_uncon <- melt(results_uncon)
results_uncon["constraint"] <- "unconstrained"

results <- rbind(results_con_sp, results_con_pi, results_con_sppi, results_uncon)

df <- results[which(results$statistic == "error" & !is.na(match(results$param, c("se1", "sp1", "pi")))),]

df <- df[which(results_uncon$prior.sd == 0.05), ]

df$constraint <- factor(df$constraint, levels = c("constrained Sp", "constrained Pi", "constrained Sp Pi", "unconstrained"))

df <- df[which(!is.na(df$value)),]

df$value = abs(df$value)
ggplot(df, aes(x=n.tests, y=value, group=param, color=param)) + theme_bw()+
  stat_summary(fun=mean, geom="line", position=position_dodge(width=0.25)) +
  stat_summary(fun=mean, geom="point", size=3, position=position_dodge(width=0.25)) +
  stat_summary(
    fun=mean,
    fun.min=function(x) pmax(mean(x) - sd(x),0), 
    fun.max=function(x) mean(x) + sd(x), 
    width=0.1,
    size=1,
    geom="errorbar",
    position=position_dodge(width=0.25)
  ) +
  xlab("Number of diagnostic tests") + 
  ylab("Absolute error") + 
  labs(col="Parameter")+
  scale_y_continuous(limits = c(0, NA))+
  facet_grid(~constraint)

