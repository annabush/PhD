"
Anna Bush

Script to illustrate how the mean value was selected for a prior distribution
"

library(ggplot2)
library(truncnorm)

truth <- 0.3
draw.sd <- 0.15
prior.mean <- rtruncnorm(1, 0, 1, truth, draw.sd)

# The peak y value of the distribution
peak <- dtruncnorm(truth, 0, 1, truth, draw.sd) 

# Lines for truth and prior mean
lines.1 <- rbind(
  data.frame(x=rep(truth, 2), y=c(0, peak), grp=c(1, 1)), 
  data.frame(x=rep(prior.mean, 2), y=c(0, dtruncnorm(truth, 0, 1, truth, draw.sd)), grp=c(2, 2))
)

# Lines for annotating draw standard deviation
y.max <- peak + 0.5
lines.2 <- rbind(
  data.frame(x=rep(truth-draw.sd, 2), y=c(0, y.max), grp=c(1, 1)) ,
  data.frame(x=rep(truth+draw.sd, 2), y=c(0, y.max), grp=c(2, 2)),
  data.frame(x=c(truth-draw.sd, truth+draw.sd), y=c(y.max-0.1, y.max-0.1), grp=c(3, 3))
)


ggplot() + theme_bw() +
  stat_function(data=data.frame(x=c(0, 1)), aes(x), fun=dtruncnorm, n=1001, args=list(a=0, b=1, mean=truth, sd=draw.sd), size = 1) +
  geom_line(data=lines.1, aes(x=x, y=y, group=grp), linetype="dashed") +
  geom_line(data=lines.2, aes(x=x, y=y, group=grp)) +
  # geom_segment(arrow = arrow(), ends = "both")+
  annotate("text", x=truth, y=y.max, label="Draw standard deviation") + #draw stnd dev
  annotate("text", x=truth, y=peak+0.1, label="Truth") + #truth
  annotate("text", x=prior.mean, y=peak+0.1, label="Prior mean") + #prior mean
  xlab("Value") + ylab("Density")

