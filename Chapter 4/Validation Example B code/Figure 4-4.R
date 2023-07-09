"
Anna Bush

Plot unconstrained results from cast study 2
"

working.directory <- dirname(rstudioapi::getActiveDocumentContext()$path)
results.path <- "../Simulated datasets 1 and 2/study 2/unconstrained/results.RData"

setwd(working.directory)

results <- readRDS(results.path)
results <- melt(results)

df <- results[which(results$statistic == "error" & !is.na(match(results$param, c("se1", "sp1", "pi")))),]
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
  facet_grid(~prior.sd)
