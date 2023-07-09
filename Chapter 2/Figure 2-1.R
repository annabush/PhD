"
Anna Bush

Script to create a Stommel diagram to illustrate the concept 
of an ecological hierarchy on a logarithm base 10 grid. 

"

library(ggplot2)
library(data.table)
library(ggforce)
library(palr)


# CHOOSE SHAPES
shapes <- list(
  shape1=list(
    x=c(-0.5, 0, 0, -0.5), 
    y=c(-2.3, -2.3, 3.7, 3.7),
    lab="Other Pathogens",
    fill="white",
    col="grey60",
    alpha=0.2
  ),
  shape2=list(
    x=c(0, 1, 1, 0), 
    y=c(-2.3, -2.3, -1, -1),
    lab="Pathogen",
    fill="white",
    col="grey60",
    alpha=0.2
  ),
  shape3=list(
    x=c(0.7, 0.6, 0.7, 1.3, 2.2, 1.1, 0.3, 0, 0.3),
    y=c(-1.4, 0, 1, 2, 3, 2, 1, 0, -1.4),
    lab="Behaviour and Movement",
    fill="steelblue4",
    col="white",
    alpha=0.2
  ),
  shape4=list(
    x=c(1, 1.2, 1.2, 0.5, 0.5, 0.7),
    y=c(-1.8, -1, 1, 1, -1, -1.5, -1.8),
    lab="Host",
    fill="white",
    col="grey60",
    alpha=0.2
  ),
  shape5=list(
    x=c(2, 2, 1, 1),
    y=c(-0.2, 0.4, 0.4, -0.2),
    lab="Condition and Traits",
    fill="blueviolet",
    col="white",
    alpha=0.2
  ),
  shape6=list(
    x=c(1.5, 1.5, 0.6, 0.6),
    y=c(0.4, 1.6, 1.6, 0.4),
    lab="Group",
    fill="white",
    col="grey60",
    alpha=0.2
  ),
  shape7=list(
    x=c(2, 1.7, 0.9, 1.3),
    y=c(0.8, 1, 0.7, 0.3),
    lab="Demographic Processes",
    fill="springgreen3",
    col="white",
    alpha=0.2
  ),
  shape8=list(
    x=c(2, 2, 1.1, 1.1),
    y=c(0.8, 2, 2, 0.8),
    lab="Population",
    fill="white",
    col="grey60",
    alpha=0.2
  ),
  shape9=list(
    x=c(1.6, 1.3, 1.1, 1.6),
    y=c(2.5, 2, 1.8, 2),
    lab="", #Range",
    fill="red3",
    col="white",
    alpha=0.2
  ),
  shape10=list(
    x=c(3, 3, 1.2, 1.2),
    y=c(0.9, 2.8, 2.8,0.9),
    lab="Metapopulation",
    fill="white",
    col="grey60",
    alpha=0.2
  ),
  shape11=list(
    x=c(4.5, 3, 2, 1.8, 2.2, 3),
    y=c(3.4, 2.5, 1.7, 1.5, 1.6, 2.3),
    lab="Dispersal and Climatic Processes",
    fill="orange2",
    col="white",
    alpha=0.2
  ),
  shape12=list(
    x=c(4, 4, 2, 2),
    y=c(1.6, 3.5, 3.5, 1.6),
    lab="Host Assemblage",
    fill="white",
    col="grey60",
    alpha=0
  ),
  shape13=list(
    x=c(3.8, 3.3, 2.5, 2, 1.1, 0.7, 1.2, 2, 3),
    y=c(2.2, 2.1, 1.2, 0.8, 0.5, -0.3, -0.7, -0.5, 0.5),
    lab="Competition",
    fill="thistle3",
    col="white",
    alpha=0.5
  )
)


format.shapes <- function(shapes){
  # FORMAT A LIST OF SHAPES INTO A DATAFRAME FOR GGPLOT
  dat <- data.frame()
  for (i in 1:length(shapes)){
    dat <- rbind(dat, format.shape(shapes[[i]], i))
  }
  return(dat)
}


make.names <- function(ni){
  nx <- c()
  ny <- c() 
  for (i in 1:ni){
    nx = c(nx, paste("x", i, sep=""))
    ny = c(ny, paste("y", i, sep=""))
  }
  names = c(nx, ny)  
  return(names)
}

format.shape <- function(shape, id){
  # FORMT A SINGLE SHAPE INTO A DATAFRAME FOR GGPLOT
  
  ni <- min(length(shape$x), length(shape$y))  # Number of indices
  names <- make.names(ni)  # (x1, x2, ..., y1, y2, ...)

  # Initialise array
  dat <- array(
    NA, 
    dim=c(1, ni*2),
    dimnames=list(shape=c(1), value=c(names))
  )
  
  # Populate array
  for (i in 1:ni){
    dat[1, i] <- shape$x[i]
  }
  for (i in 1:ni){
    dat[1, i+ni] <- shape$y[i]
  }
  dat <- data.frame(dat)
  dat$ID <- id
  dat <- melt(setDT(dat), measure=patterns(c("^x","^y")), value.name=c("x","y"))
  return(dat)
}

dat <- format.shapes(shapes)


# Function to map second y-axis labels
g <- function(x){
  y <- c("Minutes", "Hours", "Days", "Month", "Year", "Decade", "Century")
  return(y[x + 3])
}

# Function to change alpha of color
include.alpha <- function(color, alpha) {
  color <- c(paste(as.vector(col2rgb(color))), 255 * alpha)
  return(rgb(color[1], color[2], color[3], color[4], maxColorValue=255))
}


# MAKE ANNOTATION DATAFRAME
ann = array(
  NA, 
  dim=c(length(shapes), 4), 
  dimnames=list(shape=1:length(shapes), var=c("ID", "lab", "x", "y"))
)
for (i in 1:length(shapes)){
  # df <- dat[which(dat$ID == i),]
  ann[i, 1] <- i  # ID
  ann[i, 2] <- shapes[[i]]$lab[1]  # 
  ann[i, 3] <- as.numeric(mean(shapes[[i]]$x))
  ann[i, 4] <- max(shapes[[i]]$y) + 0.1
}
ann <- as.data.frame(ann)
ann$ID <- as.numeric(ann$ID)
ann$x <- as.numeric(ann$x)
ann$y<- as.numeric(ann$y)

fill <- c()
col <- c()
alpha <- c()
for (shape in shapes){
  fill <- c(fill, include.alpha(shape$fill, shape$alpha))
  col <- c(col, shape$col)
}

# PLOT
p <- ggplot(dat) +
  geom_shape(aes(x=x, y=y, group=ID, col=factor(ID), fill=factor(ID)), radius = unit(0.2, 'cm'), size=.75) +
  scale_x_continuous(name="Budget of susceptible organisms", breaks=seq(0, 4, by=1),sec.axis=sec_axis(~., labels=function(x){10^x})) +
  scale_y_continuous(name="Timeframe", breaks=seq(-2, 4, by=1), sec.axis=sec_axis(~., breaks=seq(-2, 4, by=1), labels=g)) +
  #annotate("text", x=ann$x, y=ann$y, label=ann$lab) +
  scale_fill_manual(values=fill) +
  scale_color_manual(values=col) +
  theme(panel.background = element_rect(fill="white", colour="grey50"), legend.position="none", 
        panel.grid.major = element_line(colour="grey90", size=0.6),
        panel.border = element_rect(fill=NA,color="gray50", size=0.5, linetype="solid"),
        axis.text = element_text(color="black", size=12,face="bold"),
        axis.title=element_text(size=12,face="bold"))

p + annotate("text", x = .5, y = -2, label = "PATHOGEN\nINDIVIDUAL",fontface="bold") + 
  annotate("text", x = -.3, y = 1, label = "POOL OF POTENTIALLY PATHOGENIC ORGANISMS",fontface="bold", angle=90)+
  annotate("text", x = .9, y = -.3, label = "HOST INDIVIDUAL",fontface="bold")+
  annotate("text", x = 1, y = 1.2, label = "GROUP",fontface="bold")+
  annotate("text", x = 1.6, y = 1.8, label = "POPULATION",fontface="bold")+
  annotate("text", x = 1.9, y = 2.6, label = "METAPOPULATION",fontface="bold")+
  annotate("text", x = 2.7, y = 3.4, label = "HOST ASSEMBLAGE",fontface="bold")+
  annotate("segment", x = 3, xend = 3.5, y = 0.5, yend = -.2, colour="grey40", size=.75, lineend = "round")+
  annotate("text", x = 3.6, y = -.3, label = "competition",fontface="bold", colour="grey40")+
  annotate("segment", x = 1.9, xend = 2.2, y = -.1, yend = -.4, colour="grey40", size=.75)+
  annotate("text", x = 2.3, y = -.5, label = "condition and traits",fontface="bold", colour="grey40")+
  annotate("segment", x = 1.7, xend = 1.7, y = 1, yend = 1.3, colour="grey40", size=.75)+
  annotate("text", x = 2, y = 1.5, label = "demographic processes",fontface="bold", colour="grey40")+
  annotate("segment", x = 3.1, xend = 3.1, y = 2.5, yend = 2.9, colour="grey40", size=.75)+
  annotate("text", x = 3.7, y = 3, label = "dispersal and climatic processes",fontface="bold", colour="grey40")+
  annotate("segment", x = 1.4, xend = 1.2, y = 2.1, yend = 3, colour="grey40", size=.75)+
  annotate("text", x = 1.1, y = 3.1, label = "range",fontface="bold", colour="grey40")+
  annotate("segment", x = .35, xend = .35, y = 1, yend = 2.5, colour="grey40", size=.75)+
  annotate("text", x = .45, y = 2.75, label = "behaviour\nand movement",fontface="bold", colour="grey40")+
  annotate("segment", x = .5, xend = 3.7, y = -1.7, yend = 2.7, colour = "darkred", size=.75, arrow = arrow(type = "closed"))+
  annotate("text", x = 2, y = 0.2, label = "ABUNDANCE OF TRANSMISSION EVENTS",fontface="bold", angle=45, colour="darkred")
  

