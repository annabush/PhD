"
Anna Bush
"

library(tidyr)
library(lme4)
library(lmerTest)
library(effects)
library(sjPlot)

working.directory <- paste(dirname(rstudioapi::getActiveDocumentContext()$path), "Fitted v Residual plots", sep="/")
create.dir(working.directory, recursive = TRUE)
setwd(working.directory)

# B. run the relevant LMMs (4 conditions, 3 methods, for 4 error parameters each condition x method)

#there are 3 methods:
# 1.prod
# 2.edge 1
# 3.edge 2

# there are 4 conditions:
# no logit, no abs = LMM1, 31, 25, 28 -  error
# logit, no abs = m1a, 1b, 1c, 1d - error 2
# logit, abs = m1e, 1f, 1g, 1h  -  error3
# no logit, abs = LMM2, 32, 26, 29  - error5

#there are 4 error parameters:
#global
#pi
#se
#sp

var_names <- c(
  "error", "error.pi", "error.se1", "error.sp1",
  "error2", "error2.pi", "error2.se1", "error2.sp1",
  "error3", "error3.pi", "error3.se1", "error3.sp1",
  "error5", "error5.pi", "error5.se1", "error5.sp1"
)

conditions <- c(
  rep("NOlogit-NOabs", 4),
  rep("logit-NOabs", 4),
  rep("logit-abs", 4),
  rep("NOlogit-abs", 4)
) 


reformat <- function(lmm, data){
  f <- unlist(fitted(lmm))
  r <- residuals(lmm)
  
  n <- length(f)
  pi <- data$pi
  se <- data$se1
  sp <- data$sp1
  
  modc <- array(
    data=c(f, r), 
    dim=c(n,2), 
    dimnames=list(row=1:n, param=c("fitted", "residual"))
  )
  
  # define the edges of parameter space
  modc <- as.data.frame(modc)
  modc["near_edge"] <- se > 0.9 | se < 0.1 | sp > 0.9 | pi < 0.1
  
  # define where the edges are within parameter space
  modc["which_edge"] <- "middle"
  modc["which_edge"][which(se > 0.9 | sp > 0.9),] <- "upper"
  modc["which_edge"][which(se < 0.1 | pi < 0.1), ] <- "lower"
  
  return(modc)
}

run_model <- function(response_variable, data){
  require(lme4)
  right_hand_side <- "prior.precision + constraint + n.badgers + n.tests * extreme + (1|se1) + (1|sp1) + (1|pi)"
  formula <- as.formula(paste0(response_variable, " ~ ", right_hand_side))
  lmm <- lmer(formula, data=data)
  return (lmm)
}


data <- results[which(results$prior.distribution != "Uniform"),]

for (i in 1:length(var_names)){

  lmm <- run_model(var_names[i], data)
  lmm <- reformat(lmm, data)
  
  # edge method (only colouring residuals when they occur at edge of parameter space BY CATEGORY)
  # near_edge
  g <- ggplot(lmm, aes(fitted, residual)) + 
    geom_point(aes(colour=factor(near_edge)), alpha = 1/5) +
    geom_hline(yintercept=0, linetype="dashed")+
    geom_vline(xintercept=0)+
    facet_grid(rows=vars(near_edge))
  ggsave(filename=paste(var_names[i], "_edge1_", conditions[i], ".png", sep=""), plot=g, width=5.9)
  
  #edge method (only colouring residuals when they occur at edge of parameter space)
  # which_edge
  g <- ggplot(lmm, aes(fitted, residual)) + 
    geom_point(aes(colour=factor(which_edge)), alpha = 1/5) +
    geom_hline(yintercept=0, linetype="dashed")+
    geom_vline(xintercept=0)+
    facet_grid(rows=vars(which_edge))
  ggsave(filename=paste(var_names[i], "_edge2_", conditions[i], ".png", sep=""), plot=g, width=5.9)
  
}






#no logit, non-absolute errors, global
LMM1 <- lmer(error ~ prior.precision + constraint + n.badgers + n.tests* extreme + 
               (1|se1) + (1|sp1) + (1|pi),
             data = results[which(results$prior.distribution != "Uniform"),])

#no logit, non-absolute errors, pi
LMM31 <- lmer(error.pi ~ prior.precision + constraint + n.badgers + n.tests* extreme + 
                (1|se1) + (1|sp1) + (1|pi),
              data = results[which(results$prior.distribution != "Uniform"),])

#no logit, non-absolute errors, se
LMM25 <- lmer(error.se1 ~ prior.precision + constraint + n.badgers + n.tests* extreme + 
                (1|se1) + (1|sp1) + (1|pi),
              data = results[which(results$prior.distribution != "Uniform"),])

#no logit, non-absolute errors, sp
LMM28 <- lmer(error.sp1 ~ prior.precision + constraint + n.badgers + n.tests* extreme + 
                (1|se1) + (1|sp1) + (1|pi),
              data = results[which(results$prior.distribution != "Uniform"),])

#no logit, absolute errors, global
LMM2 <- lmer(error5 ~ prior.precision + constraint + n.badgers + n.tests* extreme + 
               (1|se1) + (1|sp1) + (1|pi),
             data = results[which(results$prior.distribution != "Uniform"),])

#no logit, absolute errors, pi
LMM32 <- lmer(error5.pi ~ prior.precision + constraint + n.badgers + n.tests* extreme + 
                (1|se1) + (1|sp1) + (1|pi),
              data = results[which(results$prior.distribution != "Uniform"),])

#no logit, absolute errors, se
LMM26 <- lmer(error5.se1 ~ prior.precision + constraint + n.badgers + n.tests* extreme + 
                (1|se1) + (1|sp1) + (1|pi),
              data = results[which(results$prior.distribution != "Uniform"),])

#no logit, absolute errors, sp
LMM29 <- lmer(error5.sp1 ~ prior.precision + constraint + n.badgers + n.tests* extreme + 
                (1|se1) + (1|sp1) + (1|pi),
              data = results[which(results$prior.distribution != "Uniform"),])

# logit, no abs, global
m1a <- lmer(error2 ~ prior.precision + constraint + n.badgers + n.tests * extreme + 
              (1|se1) + (1|sp1) + (1|pi),
            data = results[which(results$prior.distribution != "Uniform"),])

# logit, no abs, pi
m1b <- lmer(error2.pi ~ prior.precision + constraint + n.badgers + n.tests * extreme + 
              (1|se1) + (1|sp1) + (1|pi),
            data = results[which(results$prior.distribution != "Uniform"),])

# logit, no abs, se1
m1c <- lmer(error2.se1 ~ prior.precision + constraint + n.badgers + n.tests * extreme + 
              (1|se1) + (1|sp1) + (1|pi),
            data = results[which(results$prior.distribution != "Uniform"),])

# logit, no abs, pi
m1d <- lmer(error2.sp1 ~ prior.precision + constraint + n.badgers + n.tests * extreme + 
              (1|se1) + (1|sp1) + (1|pi),
            data = results[which(results$prior.distribution != "Uniform"),])

# logit, abs, global
m1e <- lmer(error3 ~ prior.precision + constraint + n.badgers + n.tests * extreme + 
              (1|se1) + (1|sp1) + (1|pi),
            data = results[which(results$prior.distribution != "Uniform"),])

# logit, abs, pi
m1f <- lmer(error3.pi ~ prior.precision + constraint + n.badgers + n.tests * extreme + 
              (1|se1) + (1|sp1) + (1|pi),
            data = results[which(results$prior.distribution != "Uniform"),])

# logit, abs, se1
m1g <- lmer(error3.se1 ~ prior.precision + constraint + n.badgers + n.tests * extreme + 
              (1|se1) + (1|sp1) + (1|pi),
            data = results[which(results$prior.distribution != "Uniform"),])

# logit, abs, sp1
m1h <- lmer(error3.sp1 ~ prior.precision + constraint + n.badgers + n.tests * extreme + 
              (1|se1) + (1|sp1) + (1|pi),
            data = results[which(results$prior.distribution != "Uniform"),])
            