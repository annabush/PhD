library(ggplot2)

working.directory <- paste(dirname(rstudioapi::getActiveDocumentContext()$path), "Results", sep="/")
setwd(working.directory)

scenario <- woodchester.linear.sp
simulation <- 1

for (model.name in scenario){
  result <- readRDS(paste(model.name, "rds", sep="."))
  result["scenario"] <- model.name
  
  result <- result[result$simulation == simulation, ]
  
  if (model.name == scenario[1]){
    result[result["type"] == "true", "scenario"] = "true"
    
    results <- result
    true <- result[result$type == "true", ]
    
  } else if(all(result[result$type == "true", "value"] == true$value)){    
    results <- rbind(results, result[result$type == "predicted", ])
  }
}

ggplot(results) + 
  geom_line(aes(time.step, value, colour=scenario), size=1) + 
  scale_x_continuous(limits = c(1, 10), breaks = seq(1, 10, 1))+
  facet_grid(parameter ~ index)
