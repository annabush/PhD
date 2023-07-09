library(scales)

reg.summary <- function(model, model.name, response.var, out.file, digits=3){
  write(sprintf(",%s", model.name), out.file)
  write("Fixed regression coefficients (SE),", out.file, append = T)
  
  fixed.eff <- fixef(model)
  cov.mat <- vcov(model)
  
  for (i in 1:nrow(cov.mat)){
    coeff.name <- gsub("\\[|\\]", "", c(rownames(cov.mat)[i]))
    write(
      sprintf(
        "%s,%s (%s)", 
        coeff.name, 
        scientific(fixed.eff[i], digits), 
        scientific(sqrt(cov.mat[i, i]), digits)
      ), out.file, append = T
    )
  }
  write("Random effects variances (SD),", out.file, append = T)
  
  rand.eff <- as.data.frame(VarCorr(model))
  for (i in 1:nrow(rand.eff)){
    write(
      sprintf(
        "%s,%s (%s)", 
        rand.eff[i, 1], 
        scientific(rand.eff[i, 4], digits), 
        scientific(sqrt(rand.eff[i, 4]), digits)
      ), out.file, append = T
    )
  }
  write(sprintf("Number of data points,%s", nobs(model)), out.file, append = T)
}

reg.summary(LMM42, "LMM42", "error", "results.csv")


