library(ggplot2)

working.directory <- paste(dirname(rstudioapi::getActiveDocumentContext()$path), "Results", sep="/")
setwd(working.directory)

# Random truths
scenario.1 <- c(
  "random_independent",
  "random_constant",
  "random_linear"
)

# Constant truths
scenario.2 <- c(
  "constant_linear",
  "constant_constant",
  "constant_independent"
)

# Constant noisy truths
scenario.3 <- c(
  "constant-noisy002_independent",
  "constant-noisy002_constant",
  "constant-noisy002_linear"
)

# Linear truths
scenario.4 <- c(
  "linear_independent",
  "linear_constant",
  "linear_linear"
)

# Linear noisy truths
scenario.5 <- c(
  "linear-noisy002_independent",
  "linear-noisy002_constant",
  "linear-noisy002_linear"
)

# mixed truths
scenario.6 <- c(
  "mixed_independent",
  "mixed_constant",
  "mixed_linear",
  "mixed_mixed"
)

# mixed truths
scenario.7 <- c(
  "mixed-noisy002_independent",
  "mixed-noisy002_constant",
  "mixed-noisy002_linear",
  "mixed-noisy002_mixed"
)

scenario.8 <- c(
  "mixed-noisy002_mixed_02",
  "mixed-noisy002_mixed_03",
  "mixed-noisy002_mixed_04",
  "mixed-noisy002_mixed",
  "mixed-noisy002_mixed_07",
  "mixed-noisy002_mixed_08",
  "mixed-noisy002_mixed_09",
  "mixed-noisy002_mixed_10"
)


woodchester.simple <- c(
  "woodchester_independent",
  "woodchester_constant",
  "woodchester_linear"
)

woodchester.independent.pi <- c(
  "woodchester_independent",
  "woodchester_independent_independent_linear",
  "woodchester_independent_linear_independent",
  "woodchester_independent_linear_linear"
)

woodchester.linear.pi <- c(
  "woodchester_linear_independent_independent",
  "woodchester_linear_independent_linear",
  "woodchester_linear_linear_independent",
  "woodchester_linear"
)


woodchester.independent.se <- c(
  "woodchester_independent",
  "woodchester_independent_independent_linear",
  "woodchester_linear_independent_independent",
  "woodchester_linear_independent_linear"
)

woodchester.linear.se <- c(
  "woodchester_independent_linear_independent",
  "woodchester_independent_linear_linear",
  "woodchester_linear_linear_independent",
  "woodchester_linear"
)

woodchester.independent.sp <- c(
  "woodchester_independent",
  "woodchester_independent_linear_independent",
  "woodchester_linear_independent_independent",
  "woodchester_linear_linear_independent"
)

woodchester.linear.sp <- c(
  "woodchester_independent_independent_linear",
  "woodchester_linear_independent_linear",
  "woodchester_independent_linear_linear",
  "woodchester_linear"
)


scenario <- scenario.8

parameters <- c("pi", "se1", "se2", "se3", "sp1", "sp2", "sp3")

summary <- array(
  NA, 
  dim=c(length(scenario), length(parameters) + 1), 
  dimnames=list(scenario=scenario, parameter=c(parameters, "total"))
)


for (model.name in scenario){
  result <- readRDS(paste(model.name, "rds", sep="."))
  result["scenario"] <- model.name
  true <- result[result$type == "true", ]
  pred <- result[result$type == "predicted", ]
  error <- pred
  error$value <- abs(pred$value - true$value)

  # Populate summary array
  for (parameter in parameters){
    summary[model.name, parameter] <- mean(error[error$variable == parameter, ]$value)
  }
  summary[model.name, "total"] <- mean(summary[model.name, 1:length(parameters)])
  
  if (model.name == scenario[1]){
    errors <- error
  } else {
    errors <- rbind(errors, error)
  }
}

ggplot(errors) + geom_density(aes(value, colour=parameter)) + facet_grid(scenario ~ .)

summary

