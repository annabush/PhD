# ANNA BUSH
# CODE FOR 2D AND 3D HEATMAPS

library(ggplot2)
library(rayshader)

working.directory <- paste(dirname(rstudioapi::getActiveDocumentContext()$path), "../../Heatmaps for study 1/Pi errors and precisions", sep="/")
create.dir(working.directory, recursive = TRUE)
setwd(working.directory)


# library(magick)

## set colour of heatmaps
#two_d <- c("blue", "white", "red")
#three_d <- rainbow(5)
#colours <- two_d

# set colour of heatmaps
nonabs <- c("blue", "white", "red")
#abso <- c("white", "seashell", "pink", "red")
abso <- c("green", "pink", "red")
#prec <- c("white", "honeydew", "green", "darkgreen")
prec <- c("green", "pink", "red")
#three_d <- rainbow(5)
#colours <- three_d

# plotA to plotL: 2D ggplot heatmaps of error across parameter space facetted by:

# prior precision ---------------------------------------------------------

## plotting accuracy

plotA <- ggplot(results[which(results$prior.distribution != "Uniform"),], aes(se1, sp1, z=error)) +
  geom_tile(colour = "white", binwidth=0.5/5, stat = "summary2d")+
  coord_fixed()+
  scale_y_continuous(breaks = seq(0, 1, 0.5))+
  scale_x_continuous(breaks = seq(0, 1, 0.5))+
  scale_y_continuous(sec.axis = sec_axis(~ ., name = "True Pi", breaks=NULL))+
  xlab("True Se")+
  ylab("True Sp")+
  facet_grid(pi2 ~ prior.precision)+
  scale_fill_gradientn(colours = abso, limits=c(0, 0.08), name = "Absolute Error", labels = function(x) sprintf("%.3f", x))+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank()) +
theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))
ggsave(filename="plotA.png", plot=plotA, width=5.9)

plotA1 <- ggplot(results[which(results$prior.distribution != "Uniform"),], aes(se1, sp1, z=error.pi)) +
  geom_tile(colour = "white", binwidth=0.5/5, stat = "summary2d")+
  coord_fixed()+
  scale_y_continuous(breaks = seq(0, 1, 0.5))+
  scale_x_continuous(breaks = seq(0, 1, 0.5))+
  scale_y_continuous(sec.axis = sec_axis(~ ., name = "True Pi", breaks=NULL))+
  xlab("True Se")+
  ylab("True Sp")+
  facet_grid(pi2 ~ prior.precision)+
  scale_fill_gradientn(colours = nonabs, limits=c(-0.05, 0.05), name = "Non-Absolute Error", labels = function(x) sprintf("%.3f", x))+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank()) +
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))
ggsave(filename="plotA1.png", plot=plotA1, width=5.9)

## plotting precision

plotB <- ggplot(results[which(results$prior.distribution != "Uniform"),], aes(se1, sp1, z=sd.posterior)) +
  geom_tile(colour = "white", binwidth=0.5/5, stat = "summary2d")+
  coord_fixed()+
  scale_y_continuous(breaks = seq(0.5, 1, 0.1))+
  scale_x_continuous(breaks = seq(0, 1, 0.1))+
  scale_y_continuous(sec.axis = sec_axis(~ ., name = "True Pi", breaks=NULL))+
  xlab("True Se")+
  ylab("True Sp")+
  facet_grid(pi2 ~ prior.precision)+
  scale_fill_gradientn(colours = prec, name = "SD", labels = function(x) sprintf("%.2f", x))+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank()) +
theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))
ggsave(filename="plotB.png", plot=plotB, width=5.9)


# constraint - normal -----------------------------------------------------

#plotting accuracy
plotC <- ggplot(results[which(results$prior.distribution != "Uniform"),], aes(se1, sp1, z=error.pi) ) +
  geom_tile(colour = "white", binwidth=0.5/5, stat = "summary2d")+
  coord_fixed()+
  scale_y_continuous(breaks = seq(0.5, 1, 0.1))+
  scale_x_continuous(breaks = seq(0, 1, 0.1))+
  scale_y_continuous(sec.axis = sec_axis(~ ., name = "True Pi", breaks=NULL))+
  xlab("True Se")+
  ylab("True Sp")+
  facet_grid(pi2 ~ constraint)+
  scale_fill_gradientn(colours = abso, limits=c(0, 0.13), name = "Absolute Error", labels = function(x) sprintf("%.2f", x))+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank()) +
theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))
ggsave(filename="plotC.png", plot=plotC, width=5.9)

plotC1 <- ggplot(results[which(results$prior.distribution != "Uniform"),], aes(se1, sp1, z=error.pi) ) +
  geom_tile(colour = "white", binwidth=0.5/5, stat = "summary2d")+
  coord_fixed()+
  scale_y_continuous(breaks = seq(0.5, 1, 0.1))+
  scale_x_continuous(breaks = seq(0, 1, 0.1))+
  scale_y_continuous(sec.axis = sec_axis(~ ., name = "True Pi", breaks=NULL))+
  xlab("True Se")+
  ylab("True Sp")+
  facet_grid(pi2 ~ constraint)+
  scale_fill_gradientn(colours = nonabs, limits=c(-0.04, 0.04), name = "Non-Absolute Error", labels = function(x) sprintf("%.2f", x))+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank()) +
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))
ggsave(filename="plotC1.png", plot=plotC1, width=5.9)


#plotting precision

plotD <- ggplot(results[which(results$prior.distribution != "Uniform"),], aes(se1, sp1, z=sd.pi) ) +
  geom_tile(colour = "white", binwidth=0.5/5, stat = "summary2d")+
  coord_fixed()+
  scale_y_continuous(breaks = seq(0.5, 1, 0.1))+
  scale_x_continuous(breaks = seq(0, 1, 0.1))+
  scale_y_continuous(sec.axis = sec_axis(~ ., name = "True Pi", breaks=NULL))+
  xlab("True Se")+
  ylab("True Sp")+
  facet_grid(pi2 ~ constraint)+
  scale_fill_gradientn(colours = prec, name = "SD", labels = function(x) sprintf("%.2f", x))+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank()) +
theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))
ggsave(filename="plotD.png", plot=plotD, width=5.9)

# constraint - uniform ----------------------------------------------------

#plotting accuracy

plotE <- ggplot(results[which(results$prior.distribution != "Normal"),], aes(se1, sp1, z=error.pi) ) +
  geom_tile(colour = "white", binwidth=0.5/5, stat = "summary2d")+
  coord_fixed()+
  scale_y_continuous(breaks = seq(0.5, 1, 0.1))+
  scale_x_continuous(breaks = seq(0, 1, 0.1))+
  scale_y_continuous(sec.axis = sec_axis(~ ., name = "True Pi", breaks=NULL))+
  xlab("True Se")+
  ylab("True Sp")+
  facet_grid(pi2 ~ constraint)+
  scale_fill_gradientn(colours = abso, limits=c(0, 0.6), name = "Absolute Error", labels = function(x) sprintf("%.2f", x))+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank()) +
theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))
ggsave(filename="plotE.png", plot=plotE, width=5.9)

plotE1 <- ggplot(results[which(results$prior.distribution != "Normal"),], aes(se1, sp1, z=error.pi) ) +
  geom_tile(colour = "white", binwidth=0.5/5, stat = "summary2d")+
  coord_fixed()+
  scale_y_continuous(breaks = seq(0.5, 1, 0.1))+
  scale_x_continuous(breaks = seq(0, 1, 0.1))+
  scale_y_continuous(sec.axis = sec_axis(~ ., name = "True Pi", breaks=NULL))+
  xlab("True Se")+
  ylab("True Sp")+
  facet_grid(pi2 ~ constraint)+
  scale_fill_gradientn(colours = nonabs, limits=c(-0.7, 0.7), name = "Non-Absolute Error", labels = function(x) sprintf("%.2f", x))+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank()) +
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))
ggsave(filename="plotE1.png", plot=plotE1, width=5.9)

#plotting precision

plotF <- ggplot(results[which(results$prior.distribution != "Normal"),], aes(se1, sp1, z=sd.pi) ) +
  geom_tile(colour = "white", binwidth=0.5/5, stat = "summary2d")+
  coord_fixed()+
  scale_y_continuous(breaks = seq(0.5, 1, 0.1))+
  scale_x_continuous(breaks = seq(0, 1, 0.1))+
  scale_y_continuous(sec.axis = sec_axis(~ ., name = "True Pi", breaks=NULL))+
  xlab("True Se")+
  ylab("True Sp")+
  facet_grid(pi2 ~ constraint)+
  scale_fill_gradientn(colours = prec, name = "SD", labels = function(x) sprintf("%.2f", x))+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank()) +
theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))
ggsave(filename="plotF.png", plot=plotF, width=5.9)

# n.samples ---------------------------------------------------------------

# plotting accuracy

plotG <- ggplot(results[which(results$prior.distribution != "Uniform"),], aes(se1, sp1, z=error.pi) ) +
  geom_tile(colour = "white", binwidth=0.5/5, stat = "summary2d")+
  coord_fixed()+
  scale_y_continuous(breaks = seq(0.5, 1, 0.1))+
  scale_x_continuous(breaks = seq(0, 1, 0.2))+
  scale_y_continuous(sec.axis = sec_axis(~ ., name = "True Pi", breaks=NULL))+
  xlab("True Se")+
  ylab("True Sp")+
  facet_grid(pi2 ~ n.badgers)+
  scale_fill_gradientn(colours = abso, limits=c(0, 0.05), name = "Absolute Error", labels = function(x) sprintf("%.2f", x))+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank()) +
theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))
ggsave(filename="plotG.png", plot=plotG, width=5.9)

plotG1 <- ggplot(results[which(results$prior.distribution != "Uniform"),], aes(se1, sp1, z=error.pi) ) +
  geom_tile(colour = "white", binwidth=0.5/5, stat = "summary2d")+
  coord_fixed()+
  scale_y_continuous(breaks = seq(0.5, 1, 0.1))+
  scale_x_continuous(breaks = seq(0, 1, 0.2))+
  scale_y_continuous(sec.axis = sec_axis(~ ., name = "True Pi", breaks=NULL))+
  xlab("True Se")+
  ylab("True Sp")+
  facet_grid(pi2 ~ n.badgers)+
  scale_fill_gradientn(colours = nonabs, limits=c(-0.05, 0.05), name = "Non-Absolute Error", labels = function(x) sprintf("%.2f", x))+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank()) +
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))
ggsave(filename="plotG1.png", plot=plotG1, width=5.9)

#plotting precision

plotH <- ggplot(results[which(results$prior.distribution != "Uniform"),], aes(se1, sp1, z=sd.pi) ) +
  geom_tile(colour = "white", binwidth=0.5/5, stat = "summary2d")+
  coord_fixed()+
  scale_y_continuous(breaks = seq(0.5, 1, 0.1))+
  scale_x_continuous(breaks = seq(0, 1, 0.2))+
  scale_y_continuous(sec.axis = sec_axis(~ ., name = "True Pi", breaks=NULL))+
  xlab("True Se")+
  ylab("True Sp")+
  facet_grid(pi2 ~ n.badgers)+
  scale_fill_gradientn(colours = prec, name = "SD", labels = function(x) sprintf("%.2f", x))+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank()) +
theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))
ggsave(filename="plotH.png", plot=plotH, width=5.9)

# n.tests -----------------------------------------------------------------

#plotting accuracy

plotI <- ggplot(results[which(results$prior.distribution != "Uniform"),], aes(se1, sp1, z=error.pi) ) +
  geom_tile(colour = "white", binwidth=0.5/5, stat = "summary2d")+
  coord_fixed()+
  scale_y_continuous(breaks = seq(0.5, 1, 0.1))+
  scale_x_continuous(breaks = seq(0, 1, 0.2))+
  scale_y_continuous(sec.axis = sec_axis(~ ., name = "True Pi", breaks=NULL))+
  xlab("True Se")+
  ylab("True Sp")+
  facet_grid(pi2 ~ n.tests)+
  scale_fill_gradientn(colours = abso, limits=c(0, 0.065), name = "Absolute Error", labels = function(x) sprintf("%.2f", x))+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank()) +
theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))
ggsave(filename="plotI.png", plot=plotI, width=5.9)

plotI1 <- ggplot(results[which(results$prior.distribution != "Uniform"),], aes(se1, sp1, z=error.pi) ) +
  geom_tile(colour = "white", binwidth=0.5/5, stat = "summary2d")+
  coord_fixed()+
  scale_y_continuous(breaks = seq(0.5, 1, 0.1))+
  scale_x_continuous(breaks = seq(0, 1, 0.2))+
  scale_y_continuous(sec.axis = sec_axis(~ ., name = "True Pi", breaks=NULL))+
  xlab("True Se")+
  ylab("True Sp")+
  facet_grid(pi2 ~ n.tests)+
  scale_fill_gradientn(colours = nonabs, limits=c(-0.065, 0.065), name = "Non-Absolute Error", labels = function(x) sprintf("%.2f", x))+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank()) +
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))
ggsave(filename="plotI1.png", plot=plotI1, width=5.9)

#plotting precision

plotJ <- ggplot(results[which(results$prior.distribution != "Uniform"),], aes(se1, sp1, z=sd.pi) ) +
  geom_tile(colour = "white", binwidth=0.5/5, stat = "summary2d")+
  coord_fixed()+
  scale_y_continuous(breaks = seq(0.5, 1, 0.1))+
  scale_x_continuous(breaks = seq(0, 1, 0.2))+
  scale_y_continuous(sec.axis = sec_axis(~ ., name = "True Pi", breaks=NULL))+
  xlab("True Se")+
  ylab("True Sp")+
  facet_grid(pi2 ~ n.tests)+
  scale_fill_gradientn(colours = prec, name = "SD", labels = function(x) sprintf("%.2f", x))+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank()) +
theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))
ggsave(filename="plotJ.png", plot=plotJ, width=5.9)

# prior distribution (normal v uniform) -----------------------------------

#plotting accuracy

plotK <- ggplot(results[which(results$prior.precision != "Precise"),], aes(se1, sp1, z=error.pi) ) +
  geom_tile(colour = "white", binwidth=0.5/5, stat = "summary2d")+
  coord_fixed()+
  scale_y_continuous(breaks = seq(0.5, 1, 0.1))+
  scale_x_continuous(breaks = seq(0, 1, 0.2))+
 scale_y_continuous(sec.axis = sec_axis(~ ., name = "True Pi", breaks=NULL))+
  xlab("True Se")+
  ylab("True Sp")+
  facet_grid(pi2 ~ prior.distribution)+
  scale_fill_gradientn(colours = abso, limits=c(0, 0.35), name = "Absolute Error", labels = function(x) sprintf("%.2f", x))+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank()) +
theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))
ggsave(filename="plotK.png", plot=plotK, width=5.9)

plotK1 <- ggplot(results[which(results$prior.precision != "Precise"),], aes(se1, sp1, z=error.pi) ) +
  geom_tile(colour = "white", binwidth=0.5/5, stat = "summary2d")+
  coord_fixed()+
  scale_y_continuous(breaks = seq(0.5, 1, 0.1))+
  scale_x_continuous(breaks = seq(0, 1, 0.2))+
  scale_y_continuous(sec.axis = sec_axis(~ ., name = "True Pi", breaks=NULL))+
  xlab("True Se")+
  ylab("True Sp")+
  facet_grid(pi2 ~ prior.distribution)+
  scale_fill_gradientn(colours = nonabs, limits=c(-0.35, 0.35), name = "Non-Absolute Error", labels = function(x) sprintf("%.2f", x))+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank()) +
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))
ggsave(filename="plotK1.png", plot=plotK1, width=5.9)

#plotting precision

plotL <- ggplot(results[which(results$prior.precision != "Precise"),], aes(se1, sp1, z=sd.pi) ) +
  geom_tile(colour = "white", binwidth=0.5/5, stat = "summary2d")+
  coord_fixed()+
  scale_y_continuous(breaks = seq(0.5, 1, 0.1))+
  scale_x_continuous(breaks = seq(0, 1, 0.2))+
  scale_y_continuous(sec.axis = sec_axis(~ ., name = "True Pi", breaks=NULL))+
  xlab("True Se")+
  ylab("True Sp")+
  facet_grid(pi2 ~ prior.distribution)+
  scale_fill_gradientn(colours = prec, name = "SD", labels = function(x) sprintf("%.2f", x))+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank()) +
theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))
ggsave(filename="plotL.png", plot=plotL, width=5.9)


# code to plot 3D versions of above ggplots

# first, set colours to three_d and re-run the above sections
# insert the correct plot into the plot_gg function below

#plot_gg(plotA)

#render_snapshot(clear = TRUE)
