generate_data <- function(n, p) {
  covariates = matrix(rnorm(n*p), nrow = n, ncol = p)
  responses = rnorm(n)
  return(list(covariates = covariates, responses = responses))
}

model_select <- function(covariates, responses, cutoff) {
  summary_lm = summary(lm(responses ~ covariates))[["coefficients"]]
  p_values = summary_lm[, "Pr(>|t|)"][-1]
  keep = which(p_values <= cutoff)
  
  if (length(keep) == 0) return(vector("numeric"))
  
  new_summary = summary(lm(responses ~ covariates[,keep]))[["coefficients"]]
  new_p_values = new_summary[, "Pr(>|t|)"]
  return(new_p_values)
}
