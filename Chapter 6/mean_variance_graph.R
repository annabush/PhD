library(ggplot2)
library(cowplot)

### Uniform control
results = results[which(results$n.tests==5), ]
results = results[which(results$n.badgers==1000), ]
results = results[which(results$prior.distribution=="Uniform"), ]


#graph to plot mean-variance relationship of BLCM posteriors
#only change x to: m.pi / m.se1 / m.sp1 for parameter specific posteriors OR an error metric
#x can be global mean (or mean parameter, since that is what is plotted on heatmap)
#conditions can be changed above
A <- ggplot(data=results, aes(x=m.pi, y=sd.pi^2)) +
  geom_point() +
  geom_smooth(level=0, size=2)

B <- ggplot(data=results, aes(x=m.se1, y=sd.se1^2)) +
  geom_point() +
  geom_smooth(level=0, size=2) 

C <- ggplot(data=results, aes(x=m.sp1, y=sd.sp1^2)) +
  geom_point() +
  geom_smooth(level=0, size=2) 

plot_grid(A, B, C)



results = results[which(results$n.tests==3), ]
results = results[which(results$n.badgers==1500), ]
results = results[which(results$prior.distribution=="Normal"), ]

results = results[which(results$prior.precision=="Precise"), ]
results = results[which(results$constraint=="Constrained"), ]

#graph to plot mean-variance relationship of BLCM posteriors
#only change x to: m.pi / m.se1 / m.sp1 for parameter specific posteriors OR an error metric
#x can be global mean (or mean parameter, since that is what is plotted on heatmap)
#conditions can be changed above
A <- ggplot(data=results, aes(x=m.pi, y=sd.pi^2)) +
  geom_point() +
  geom_smooth(level=0, size=2)

B <- ggplot(data=results, aes(x=m.se1, y=sd.se1^2)) +
  geom_point() +
  geom_smooth(level=0, size=2) 

C <- ggplot(data=results, aes(x=m.sp1, y=sd.sp1^2)) +
  geom_point() +
  geom_smooth(level=0, size=2) 

plot_grid(A, B, C)


library(ggplot2)
library(cowplot)


results = results[which(results$n.tests==3), ]
results = results[which(results$n.badgers==1000), ]
results = results[which(results$prior.distribution=="Normal"), ]
results = results[which(results$prior.precision=="Precise"), ]
results = results[which(results$constraint=="Unconstrained"), ]

A <- ggplot(data=results, aes(x=m.pi, y=sd.pi^2)) +
  geom_point() +
  geom_smooth(level=0, size=2)

B <- ggplot(data=results, aes(x=m.se1, y=sd.se1^2)) +
  geom_point() +
  geom_smooth(level=0, size=2) 

C <- ggplot(data=results, aes(x=m.sp1, y=sd.sp1^2)) +
  geom_point() +
  geom_smooth(level=0, size=2) 

plot_grid(A, B, C)


library(ggplot2)
library(cowplot)


results = results[which(results$n.tests==3), ]
results = results[which(results$n.badgers==1000), ]
results = results[which(results$prior.distribution=="Normal"), ]
results = results[which(results$prior.precision=="Imprecise"), ]
results = results[which(results$constraint=="Constrained"), ]

A <- ggplot(data=results, aes(x=m.pi, y=sd.pi^2)) +
  geom_point() +
  geom_smooth(level=0, size=2)

B <- ggplot(data=results, aes(x=m.se1, y=sd.se1^2)) +
  geom_point() +
  geom_smooth(level=0, size=2) 

C <- ggplot(data=results, aes(x=m.sp1, y=sd.sp1^2)) +
  geom_point() +
  geom_smooth(level=0, size=2) 

plot_grid(A, B, C)


library(ggplot2)
library(cowplot)


results = results[which(results$n.tests==3), ]
results = results[which(results$n.badgers==1000), ]
results = results[which(results$prior.distribution=="Normal"), ]
results = results[which(results$prior.precision=="Imprecise"), ]
results = results[which(results$constraint=="Unconstrained"), ]

A <- ggplot(data=results, aes(x=m.pi, y=sd.pi^2)) +
  geom_point() +
  geom_smooth(level=0, size=2)

B <- ggplot(data=results, aes(x=m.se1, y=sd.se1^2)) +
  geom_point() +
  geom_smooth(level=0, size=2) 

C <- ggplot(data=results, aes(x=m.sp1, y=sd.sp1^2)) +
  geom_point() +
  geom_smooth(level=0, size=2) 

plot_grid(A, B, C)