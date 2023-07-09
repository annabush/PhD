"
Anna Bush

Script to plot error for prevalence, SE-1, SP-1 against the 
number of diagnostic tests.

"

working.directory <- dirname(rstudioapi::getActiveDocumentContext()$path)

results <- readRDS(paste(working.directory, results.RData, sep="/"))
results <- melt(results)

df <- results[which(results$statistic == "error" & !is.na(match(results$param, c("se1", "sp1", "pi")))),]
df$value <- abs(df$value)

p <- ggplot(df, aes(x=n.tests, y=value, group=param, color=param)) + theme_classic()+
  stat_summary(fun=mean, geom="line", size = 0.9, position=position_dodge(width=0.25)) +
  stat_summary(fun=mean, geom="point", size=3, position=position_dodge(width=0.25)) +
  stat_summary(
    fun=mean,
    fun.min=function(x) pmax(mean(x) - sd(x),0), 
    fun.max=function(x) mean(x) + sd(x), 
    width=0.1,
    size=0.9,
    geom="errorbar",
    position=position_dodge(width=0.25)
  )+
  ylab("Absolute error")+
  xlab("Number of diagnostic tests") +
  labs(col="Parameter") +
  scale_y_continuous(limits = c(0, NA))
p
# ggsave(filename="TestsvError.png")
