"
Anna Bush

Script to illustrate uniform, imprecise, and precise prior distributions
"

library(ggplot2)
library(reshape2)

truth <- 0.3
precision <- c(NA, 0.15, 0.05)
precision.lab <- c("Non-informative", "Weakly informative", "Informative")

df <- data.frame(x=c(), y=c(), precision=c())
x <- seq(0, 1, 0.001)
for (i in 1:length(precision)) {
  if (tolower(precision.lab[i]) == "non-informative") {
    y <- dunif(x, 0, 1)
  } else {
    y <- dnorm(x, truth, precision[i])
  }
  tmp <- data.frame(x=x, y=y)
  tmp["Precision"] <- precision.lab[i]
  df <- rbind(df, tmp)
}


ggplot(data=df, aes(x=x, y=y, color=Precision)) + theme_bw() +
  geom_line() +
  xlab("X") + ylab("Density")
