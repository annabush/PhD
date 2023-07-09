# ANNA BUSH
# CODE FOR 2D AND 3D HEATMAPS


library(ggplot2)
library(rayshader)
#library(magick)
library(tidyverse)
library(viridis)
library(viridisLite)

working.directory <- paste(dirname(rstudioapi::getActiveDocumentContext()$path), "../../Heatmaps for study 2/Global", sep="/")
create.dir(working.directory, recursive = TRUE)
setwd(working.directory)


# Set colour of heatmaps
nonabs <- c("blue", "white", "red")
#abso <- c("white", "seashell", "pink", "red")
abso <- c("green", "pink", "red")
#prec <- c("white", "honeydew", "green", "darkgreen")
prec <- c("green", "pink", "red")


plotA <- ggplot(results[which(results$prior.distribution != "Uniform"),], aes(se1, sp1, z=error.sp1)) +
  geom_tile(colour = "white", binwidth=.1, stat = "summary2d")+
    coord_fixed()+
  scale_y_continuous(breaks = seq(0, 1))+
  scale_x_continuous(breaks = seq(0, 1))+
    #scale_y_continuous(sec.axis = sec_axis(~ ., name = "True Pi", breaks=NULL))+
  xlab("True Se1")+
  ylab("True Sp1")+
  facet_grid(pi2 ~ prior.precision)+
   scale_fill_gradientn(colours = nonabs, limits=c(-0.12, 0.12), name = "Non-Absolute Error", labels = function(x) sprintf("%.1f", x))+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank()) +
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))
ggsave(filename="plotA.png", plot=plotA, width=5.9)

plotB <- ggplot(results[which(results$prior.distribution != "Uniform"),], aes(se1, sp1, z=error6)) +
  geom_tile(colour = "white", binwidth=.1, stat = "summary2d")+
  coord_fixed()+
  scale_y_continuous(breaks = seq(0, 1))+
  scale_x_continuous(breaks = seq(0, 1))+
  #scale_y_continuous(sec.axis = sec_axis(~ ., name = "True Pi", breaks=NULL))+
  xlab("True Se1")+
  ylab("True Sp1")+
  facet_grid(pi2 ~ prior.precision)+
  scale_fill_gradientn(colours = abso, limits=c(0, 0.08), name = "Absolute Error", labels = function(x) sprintf("%.1f", x))+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank()) +
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))
ggsave(filename="plotB.png", plot=plotB, width=5.9)


## plotting precision

plotC <- ggplot(results[which(results$prior.distribution != "Uniform"),], aes(se1, sp1, z=sd.posterior)) +
  geom_tile(colour = "white", binwidth=.1, stat = "summary2d")+
  coord_fixed()+
  scale_y_continuous(breaks = seq(0, 1, 0.5))+
  scale_x_continuous(breaks = seq(0, 1, 0.5))+
  #scale_y_continuous(sec.axis = sec_axis(~ ., name = "True Pi", breaks=NULL))+
  xlab("True Se")+
  ylab("True Sp")+
  facet_grid(pi2 ~ prior.precision)+
  scale_fill_gradientn(colours = prec, name = "SD", labels = function(x) sprintf("%.2f", x))+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank()) +
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))
ggsave(filename="plotC.png", plot=plotC, width=5.9)


# n.samples ---------------------------------------------------------------

# plotting accuracy
plotD <- ggplot(results[which(results$prior.distribution != "Uniform"),], aes(se1, sp1, z=error) ) +
  geom_tile(colour = "white", binwidth=0.1, stat = "summary2d")+
  coord_fixed()+
  scale_y_continuous(breaks = seq(0, 1, 0.5))+
  scale_x_continuous(breaks = seq(0, 1, 0.5))+
  #scale_y_continuous(sec.axis = sec_axis(~ ., name = "True Pi", breaks=NULL))+
  xlab("True Se")+
  ylab("True Sp")+
  facet_grid(pi2 ~ n.badgers)+
  scale_fill_gradientn(colours = nonabs, limits=c(-0.1, 0.1), name = "Non-Absolute Error", labels = function(x) sprintf("%.2f", x))+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank()) +
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))
ggsave(filename="plotD.png", plot=plotD, width=5.9)

plotE <- ggplot(results[which(results$prior.distribution != "Uniform"),], aes(se1, sp1, z=error6) ) +
  geom_tile(colour = "white", binwidth=0.1, stat = "summary2d")+
  coord_fixed()+
  scale_y_continuous(breaks = seq(0, 1, 0.5))+
  scale_x_continuous(breaks = seq(0, 1, 0.5))+
  #scale_y_continuous(sec.axis = sec_axis(~ ., name = "True Pi", breaks=NULL))+
  xlab("True Se")+
  ylab("True Sp")+
  facet_grid(pi2 ~ n.badgers)+
  scale_fill_gradientn(colours = abso, limits=c(0, 0.05), name = " Absolute Error", labels = function(x) sprintf("%.2f", x))+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank()) +
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))
ggsave(filename="plotE.png", plot=plotE, width=5.9)

#plotting precision

plotF <- ggplot(results[which(results$prior.distribution != "Uniform"),], aes(se1, sp1, z=sd.sp1) ) +
  geom_tile(colour = "white", binwidth=0.1, stat = "summary2d")+
  coord_fixed()+
  scale_y_continuous(breaks = seq(0, 1, 0.5))+
  scale_x_continuous(breaks = seq(0, 1, 0.5))+
  #scale_y_continuous(sec.axis = sec_axis(~ ., name = "True Pi", breaks=NULL))+
  xlab("True Se")+
  ylab("True Sp")+
  facet_grid(pi2 ~ n.badgers)+
  scale_fill_gradientn(colours = prec,  name = "SD", labels = function(x) sprintf("%.2f", x))+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank()) +
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))
ggsave(filename="plotF.png", plot=plotF, width=5.9)

# n.tests -----------------------------------------------------------------

#plotting accuracy

plotG <- ggplot(results[which(results$prior.distribution != "Uniform"),], aes(se1, sp1, z=error.sp1) ) +
  geom_tile(colour = "white", binwidth=0.1, stat = "summary2d")+
  coord_fixed()+
  scale_y_continuous(breaks = seq(0, 1, 0.5))+
  scale_x_continuous(breaks = seq(0, 1, 0.5))+
  #scale_y_continuous(sec.axis = sec_axis(~ ., name = "True Pi", breaks=NULL))+
  xlab("True Se")+
  ylab("True Sp")+
  facet_grid(pi2 ~ n.tests)+
  scale_fill_gradientn(colours = nonabs, name = "Non-Absolute Error", labels = function(x) sprintf("%.2f", x))+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank()) +
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))
ggsave(filename="plotG.png", plot=plotG, width=5.9)

plotH <- ggplot(results[which(results$prior.distribution != "Uniform"),], aes(se1, sp1, z=error6) ) +
  geom_tile(colour = "white", binwidth=0.1, stat = "summary2d")+
  coord_fixed()+
  scale_y_continuous(breaks = seq(0, 1, 0.5))+
  scale_x_continuous(breaks = seq(0, 1, 0.5))+
  #scale_y_continuous(sec.axis = sec_axis(~ ., name = "True Pi", breaks=NULL))+
  xlab("True Se")+
  ylab("True Sp")+
  facet_grid(pi2 ~ n.tests)+
  scale_fill_gradientn(colours = abso, limits=c(0, 0.1), name = "Absolute Error", labels = function(x) sprintf("%.2f", x))+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank()) +
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))
ggsave(filename="plotH.png", plot=plotH, width=5.9)

#plotting precision

plotI <- ggplot(results[which(results$prior.distribution != "Uniform"),], aes(se1, sp1, z=sd.sp1) ) +
  geom_tile(colour = "white", binwidth=0.1, stat = "summary2d")+
  coord_fixed()+
  scale_y_continuous(breaks = seq(0, 1, 0.5))+
  scale_x_continuous(breaks = seq(0, 1, 0.5))+
  #scale_y_continuous(sec.axis = sec_axis(~ ., name = "True Pi", breaks=NULL))+
  xlab("True Se")+
  ylab("True Sp")+
  facet_grid(pi2 ~ n.tests)+
  scale_fill_gradientn(colours = prec, name = "SD", labels = function(x) sprintf("%.2f", x))+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank()) +
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))
ggsave(filename="plotI.png", plot=plotI, width=5.9)

# prior distribution (normal v uniform) -----------------------------------

#plotting accuracy

plotJ <- ggplot(results[which(results$prior.precision != "Precise"),], aes(se1, sp1, z=error.sp1) ) +
  geom_tile(colour = "white", binwidth=0.1, stat = "summary2d")+
  coord_fixed()+
  scale_y_continuous(breaks = seq(0, 1, 0.5))+
  scale_x_continuous(breaks = seq(0, 1, 0.5))+
 #scale_y_continuous(sec.axis = sec_axis(~ ., name = "True Pi", breaks=NULL))+
  xlab("True Se")+
  ylab("True Sp")+
  facet_grid(pi2 ~ prior.distribution)+
  scale_fill_gradientn(colours = nonabs, limits=c(-0.7, 0.7), name = "Non Absolute Error", labels = function(x) sprintf("%.2f", x))+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank()) +
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))
ggsave(filename="plotJ.png", plot=plotJ, width=5.9)

plotK <- ggplot(results[which(results$prior.precision != "Precise"),], aes(se1, sp1, z=error6.sp1) ) +
  geom_tile(colour = "white", binwidth=0.1, stat = "summary2d")+
  coord_fixed()+
  scale_y_continuous(breaks = seq(0, 1, 0.5))+
  scale_x_continuous(breaks = seq(0, 1, 0.5))+
  #scale_y_continuous(sec.axis = sec_axis(~ ., name = "True Pi", breaks=NULL))+
  xlab("True Se")+
  ylab("True Sp")+
  facet_grid(pi2 ~ prior.distribution)+
  scale_fill_gradientn(colours = abso, limits=c(0, 0.8), name = "Absolute Error", labels = function(x) sprintf("%.2f", x))+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank()) +
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))
ggsave(filename="plotK.png", plot=plotK, width=5.9)

#plotting precision

plotL <- ggplot(results[which(results$prior.precision != "Precise"),], aes(se1, sp1, z=sd.sp1) ) +
  geom_tile(colour = "white", binwidth=0.1, stat = "summary2d")+
  coord_fixed()+
  scale_y_continuous(breaks = seq(0, 1, 0.5))+
  scale_x_continuous(breaks = seq(0, 1, 0.5))+
  #scale_y_continuous(sec.axis = sec_axis(~ ., name = "True Pi", breaks=NULL))+
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

#plot_gg(PlotL)


###################

# constraint - normal -----------------------------------------------------

#plotting accuracy
plot1 <- ggplot(results[which(results$prior.distribution != "Uniform"),], aes(se1, sp1, z=error.pi) ) +
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
ggsave(filename="plot1.png", plot=plot1, width=5.9)

plot2 <- ggplot(results[which(results$prior.distribution != "Uniform"),], aes(se1, sp1, z=error.pi) ) +
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
ggsave(filename="plot2.png", plot=plot2, width=5.9)


#plotting precision

plot3 <- ggplot(results[which(results$prior.distribution != "Uniform"),], aes(se1, sp1, z=sd.pi) ) +
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
ggsave(filename="plot3.png", plot=plot3, width=5.9)

# constraint - uniform ----------------------------------------------------

#plotting accuracy

plotX <- ggplot(results[which(results$prior.distribution != "Normal"),], aes(se1, sp1, z=error) ) +
  geom_tile(colour = "white", binwidth=0.5/5, stat = "summary2d")+
  coord_fixed()+
  scale_y_continuous(breaks = seq(0.5, 1, 0.1))+
  scale_x_continuous(breaks = seq(0, 1, 0.1))+
  scale_y_continuous(sec.axis = sec_axis(~ ., name = "True Pi", breaks=NULL))+
  xlab("True Se")+
  ylab("True Sp")+
  facet_grid(pi2 ~ constraint)+
  scale_fill_gradientn(colours = abso, limits=c(0, 1), name = "Absolute Error", labels = function(x) sprintf("%.2f", x))+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank()) +
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))
ggsave(filename="plotX.png", plot=plotX, width=5.9)

plotY <- ggplot(results[which(results$prior.distribution != "Normal"),], aes(se1, sp1, z=error.pi) ) +
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

