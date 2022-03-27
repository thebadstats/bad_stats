library(tidyverse)

## Data Set

N = 47

diagnosis = c("Male", "No Comorbidity", "Diabetes", "Hypertension", "Heart Failure", "Malignancy", "COPD",
              "Cirrhosis", "CVA", "CRF", "Morbid Obesity", "Immunocomprimised", "Drug Addiction",
              "Primary Diagnosis: Pneumonia", "Primary Diagnosis: Urosepsis", "Primary Diagnosis: Primary Bacteremia",
              "Primary Diagnosis: GI/Billiary", "Primary Diagnosis: Other", "Ventilation", "Vasopressors", "AKI",
              "Positive Blood Cultures")

X_treated = c(27, 2, 16, 20, 15, 5, 8, 6, 8, 7, 6, 6, 5, 18, 11, 7, 6, 5, 22, 22, 31, 13)

X_untreated = c(23, 1, 20, 25, 16, 7, 7, 3, 5, 8, 8, 4, 5, 19, 10, 7, 6, 5, 26, 22, 30, 13)

primary = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 0, 0, 0, 0)

data = tibble(diagnosis = diagnosis, X_treated = X_treated, X_untreated = X_untreated, primary = primary)

primary_data = data %>% filter(primary == 1)

## Simulation

null_probs_empirical = function(data){ # observed probabilities of each diagnosis from data
  (data$X_treated + data$X_untreated)/(2*N)
}

fisher_test = function(x_treated, x_untreated){
  dat = matrix(c(x_treated, x_untreated, N-x_treated, N-x_untreated), nrow = 2, ncol= 2)
  return(fisher.test(dat))
}

simulate_p_dist = function(probs, n_p){ # simulate n_p p-values from multinomial given probabilities
  results = matrix(0, nrow = length(probs), ncol =n_p)
  for(j in 1:n_p){
    X_treated_sims = rmultinom(1, N, probs)
    X_untreated_sims = rmultinom(1, N, probs)
    for(i in 1:length(probs)){
      results[i, j] = fisher_test(X_treated_sims[i,1], X_untreated_sims[i,1])$p.value
    }
  }
  return(results)
}

ps = simulate_p_dist(null_probs_empirical(primary_data), n_p = 10000) # simulate 1000 p-values
mean(colSums(ps) == dim(ps)[1]) # what proportion of simulations returned all p-values = 1
