indent.lines <- function(lines, level, indent=2){
  tabs = paste(rep(" ", indent * level), collapse="")
  for (i in 1:length(lines)){
    lines[i] <- paste(tabs, lines[i], sep="")
  }
  return (lines)
}


get.eqn <- function(n.diag, variable, index="[%i]", invert=FALSE) {
  om <- get.outcome.matrix(n.diag)
  if (invert == TRUE) {
    om <- 1 - om
  }
  eqn <- matrix("", 2^n.diag, n.diag)
  for (i in 1:n.diag){
    for (j in 1:2^n.diag){
      if (om[j, i] == 1) {
        eqn[j, i] <- sprintf(paste("%s", index, sep=""), variable, i)
      } else {
        eqn[j, i] <- sprintf(paste("(1-%s", index, ")", sep=""), variable, i)
      }
    }
  }
  return (eqn)
}


set.model.pi <- function(model, relationship, level=0, indent=2){
  
  tab <- paste(rep(" ", indent), collapse="")
  
  if (relationship == "independent") {
    lines <- c(
      "for (t in 1:n.time){", 
      paste(tab, "pi.prior[t] ~ dunif(pi.limit[1], pi.limit[2])", sep=""), 
      paste(tab, "pi[t] <- pi.prior[t]"),
      "}"
    )
  } else if (relationship == "constant") {
    lines <- c(
      "pi.prior ~ dunif(pi.limit[1], pi.limit[2])",
      "pi <- pi.prior"
    )
  } else if (relationship == "linear") {
    lines <- c(
      "pi.m.prior ~ dunif(-(pi.limit[2] - pi.limit[1])/n.time, (pi.limit[2] - pi.limit[1]) / n.time)", 
      "pi.m <- pi.m.prior",
      "pi.c.prior ~ dunif(pi.limit[1] + max(0, - pi.m * (n.time - 1)), pi.limit[2] - max(0, pi.m * (n.time - 1)))",
      "pi.c <- pi.c.prior",
      "for (t in 1:n.time){",
      paste(tab, "pi[t] <- pi.m * (t - 1) + pi.c", sep=""),
      "}"
    )
  } else {
    stop(sprintf("Unknown time-pi relationship given : %s", relationship))
  }
  
  lines <- c("# Set pi", lines, "")
  lines <- indent.lines(lines, level=level, indent=indent)
  lines <- paste(lines, collapse="\n")
  model <- paste(model, lines, sep="\n")
  return (model)
}


set.model.sesp <- function(model, relationship, param.name, level=0, indent=2){
  tab <- paste(rep(" ", indent), collapse="")
  
  if (relationship == "independent") {
    lines <- c(
      "for (t in 1:n.time){", 
      paste(tab, "for (i in 1:n.diag){"),
      paste(tab, tab, param.name, ".prior[i, t] ~ dunif(", param.name, ".limit[1], ", param.name, ".limit[2])", sep=""), 
      paste(tab, tab, param.name, "[i, t] <- ", param.name, ".prior[i, t]", sep=""),
      paste(tab, "}"),
      "}"
    )
  } else if (relationship == "constant") {
    lines <- c(
      "for (i in 1:n.diag){",
      paste(tab, param.name, ".prior[i] ~ dunif(", param.name, ".limit[1], ", param.name, ".limit[2])", sep=""),
      paste(tab, param.name, "[i] <- ", param.name, ".prior[i]", sep=""),
      "}"
    )
  } else if (relationship == "linear") {
    lines <- c(
      "for (i in 1:n.diag){",
      paste(tab, param.name, ".m.prior[i] ~ dunif(-(", param.name, ".limit[2] - ", param.name, ".limit[1])  / (n.time - 1), (", param.name, ".limit[2] - ", param.name, ".limit[1] ) / (n.time - 1))", sep=""), 
      paste(tab, param.name, ".m[i] <- ", param.name, ".m.prior[i]", sep=""), 
      paste(tab, param.name, ".c.prior[i] ~ dunif(", param.name, ".limit[1] + max(0, - ", param.name, ".m[i] * (n.time - 1)), ", param.name, ".limit[2] - max(0, ", param.name, ".m[i] * (n.time - 1)))", sep=""), 
      paste(tab, param.name, ".c[i] <- ", param.name, ".c.prior[i]", sep=""), 
      paste(tab, tab, "for  (t in 1:n.time){", sep=""), 
      paste(tab, "", param.name, "[i, t] <- ", param.name, ".m[i] * (t - 1) + ", param.name, ".c[i]", sep=""), 
      paste(tab, "}", sep=""), 
      "}"
      
    )
  } else {
    stop(sprintf("Unknown time-pi relationship given : %s", relationship))
  }
  lines <- c(paste("# Set ", param.name, sep=""), lines, "")
  lines <- indent.lines(lines, level=level, indent=indent)
  lines <- paste(lines, collapse="\n")
  model <- paste(model, lines, sep="\n")
  return (model)
}


set.model <- function(scenario.name, n.diag, pi.relationship, se.relationship, sp.relationship, constant.population.size=TRUE){
  model <- "model {"
  model <- set.model.pi(model, pi.relationship, level=1, indent=2)
  model <- set.model.sesp(model, se.relationship, param.name="se", level=1, indent=2)
  model <- set.model.sesp(model, sp.relationship, param.name="sp", level=1, indent=2)
  model <- make.equations(model, n.diag, pi.relationship, se.relationship, sp.relationship, constant.population.size, level=1, indent=2)
  model <- paste(model, "}", sep="\n")
  writeLines(model, con=paste(scenario.name, "txt", sep="."))
}


get.param.names <- function(n.diag) {
  se <- rep(NA, n.diag)
  sp <- rep(NA, n.diag)
  # These data names correspond to those found in the output of the JAGS model
  for (i in 1:max(n.diag)) {
    se[i] <- paste("se", i, sep="")
    sp[i] <- paste("sp", i, sep="")
  }
  return (list(all=c("pi", se, sp), sesp=c(se, sp), se=se, sp=sp,pi="pi"))
}


make.equations <- function(model, n.diag, pi.relationship, se.relationship, sp.relationship, constatnt.population.size=TRUE, level=0, indent=2){
  
  tab <- paste(rep(" ", indent), collapse="")
  
  if (pi.relationship == "independent" | pi.relationship == "linear") {
    pi.index <- "[t]"
  } else if (pi.relationship == "constant") {
    pi.index <- ""
  } else {
    stop(sprintf("Unknown time-pi relationship given : %s", relationship))
  }
  
  if (se.relationship == "independent" | se.relationship == "linear") {
    se.index <- "[%i, t]"
  } else if (se.relationship == "constant") {
    se.index <- "[%i]"
  } else {
    stop(sprintf("Unknown time-se relationship given : %s", relationship))
  }
  
  if (sp.relationship == "independent" | sp.relationship == "linear") {
    sp.index <- "[%i, t]"
  } else if (sp.relationship == "constant") {
    sp.index <- "[%i]"
  } else {
    stop(sprintf("Unknown time-sp relationship given : %s", relationship))
  }
  
  
  se <- get.eqn(n.diag, "se", se.index)
  sp <- get.eqn(n.diag, "sp", sp.index, invert=TRUE)
  lines <- c()
  for (i in 1:2^n.diag){
    part.1 <- sprintf("pi%s", pi.index)
    part.2 <- paste(se[i,], collapse=" * ")
    part.3 <- sprintf("(1-pi%s)", pi.index)
    part.4 <- paste(sp[i,], collapse=" * ")
    eqn <- sprintf("%s * %s + %s * %s", part.1, part.2, part.3, part.4)
    lines <- c(lines, eqn)
  }
  
  for (i in 1:length(lines)){
    lines[i] <- paste("p[", i, ", t] <- ", lines[i], sep="")
  }
  
  if (constatnt.population.size) {
    final.eqn <- paste("y[t, 1:", length(lines), "] ~ dmulti(p[1:", length(lines), ", t], n)", sep="")
  } else {
    final.eqn <- paste("y[t, 1:", length(lines), "] ~ dmulti(p[1:", length(lines), ", t], n[t])", sep="")
  }
  
  
  lines <- c(lines, final.eqn)
  lines <- indent.lines(lines, level=1, indent=indent)
  lines <- c("for (t in 1:n.time){", lines, "}")
  lines<- indent.lines(lines, leve=level, indent=indent)
  lines <- paste(lines, collapse="\n")
  model <- paste(model, lines, sep="\n")
  return (model)
}



interpolate.outputs <- function(outputs, pi.relationship, se.relationship, sp.relationship, n.diag, n.time){
  
  if (pi.relationship == "constant"){
    outputs$mean[["pi"]] <- rep(outputs$mean[["pi"]], n.time)
  }
  
  for (param.name in c("se", "sp")){
    relationship <- get(paste(param.name, "relationship", sep="."))
    
    if (relationship == "constant"){
      v <- matrix(data=0, nrow=n.diag, ncol=n.time)
      for (i in 1:n.diag){
        v[i, ] <- rep(output$mean[[param.name]][i], n.time)
      }
      outputs$mean[[param.name]] <- v
    }
  }
  return(outputs)
}

get.outcome.matrix <- function(tests) {
  # Initialise the outcomes matrix with NA
  outcomes <- matrix(
    data=NA, 
    nrow=2^tests,
    ncol=tests
  )
  # Populate all possible outcomes as a factorial array. 0=negative, 1=positive
  for(i in 1:tests) {
    outcomes[,i] <- rep(rep(c(0, 1), each=nrow(outcomes)/(2^i)), 2^(i-1))
  }
  return (outcomes)
}


outputs.to.dataframe <- function(outputs){
  n.diag <- dim(outputs$mean$se)[1]
  n.time <- dim(outputs$mean$se)[2]
  data <- array(
    NA,
    dim=c(n.time, n.diag * 2 + 1),
    dimnames=list(t=seq(n.time), param=get.param.names(n.diag)$all)
  )
  
  data[, "pi"] <- outputs$mean$pi
  for (i in 1:n.diag){
    data[, paste("se", i, sep="")] <- outputs$mean$se[i, ]
    data[, paste("sp", i, sep="")] <- outputs$mean$sp[i, ]
  }
  df <- data.frame(data)
  return (df)
}

combine.truth.and.predictions <- function(true, predicted){
  n.time <- dim(true)[1]
  n.diag <- (dim(true)[2] - 1) / 2
  
  true["time.step"] <- seq(n.time)
  true["type"] <- "true"
  true <- melt(true, id=c("time.step", "type"))
  predicted["time.step"] <- seq(dim(predicted)[1])
  predicted["type"] <- "predicted"
  predicted <- melt(predicted, id=c("time.step", "type"))
  
  df <- rbind(true, predicted)
  
  df["index"] <- 0
  df["parameter"] <- 0
  for (i in 1:dim(df)[1]){
    df[i, "index"] <- get.index(df$variable[i], n.diag)
    df[i, "parameter"] <- get.param(df$variable[i])
  }
  
  return (df)
}


get.index <- function(param.name, n.diag){
  index <- 1
  param.names <- get.param.names(n.diag)
  for (i in 1:n.diag){
    if (param.name == param.names$se[i] | param.name == param.names$sp[i]){
      index <- i
      break
    }
  }
  return (index)
}


get.param <- function(param.name){
  param.name <- as.character(param.name)
  if (param.name == "pi"){
    param.name <- "pi"
  } else if (startsWith(param.name, "se")){
    param.name <- "se"
  } else if (startsWith(param.name, "sp")){
    param.name <- "sp"
  } else {
    param.name <- "None"
  }
  return (param.name)
}
