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

run_simulation <- function(n_trials, n, p, cutoff) {
  trial_results = vector(mode = "list", length = n_trials)
  p_values_results = vector(mode = "list", length = n_trials)
  for (i in 1:n_trials) {
    trial_results[[i]] = generate_data(n, p)
    p_values_results[[i]] = model_select(trial_results[[i]][["covariates"]], 
                                  trial_results[[i]][["responses"]], cutoff)
  }
  p_values_total = unlist(p_values_results)
  save(p_values_total, file = "p_values_total")
}

make_plot <- function(string) {
  p_values_total = load(file = string)
  hist(p_values_total)
}
