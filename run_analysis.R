library(data.table)
library(rhxl)
library(EpiEstim)
library(scoringRules)
library(ggplot2)
source("evaluate.R")
source("util.R")
source("model.R")


models <- list(
  list(
    desc="Basic model",
    new_cases=new_cases_poisson,
    R_func=R_latest_value),
  list(
    desc="Bsts + poisson ",
    new_cases=new_cases_poisson,
    R_func=R_semilocal),
  list(
    desc="Bsts + neg binom ",
    new_cases=new_cases_neg_binom,
    R_func=R_semilocal),
  list(
    desc="Basic Neg Binom Model",
    new_cases=new_cases_neg_binom,
    R_func=R_latest_value)

)
evaluation_steps = c(1,7 ,14, 21, 28)



data <- read_data("2019-07-04")
model_data <- prepare_data_model(data$national,
                                 data$health_zone)
results <- list()
for(model_conf in models){
  print(model_conf$desc)
  model <- fit_model(model_data$national, model_conf$desc, model_conf$new_cases, model_conf$R_func)
  national_evo <- run_model(model_data$national, model, evaluation_steps)
  hz_results <- list()

  for(zone in names(model_data$health_zone)){
    print(zone)
    if(length(model_data$health_zone[[zone]]$days) > 10){
      model <- fit_model(model_data$health_zone[[zone]], model_conf$desc,
                         model_conf$new_cases, model_conf$R_func)
      evo <- run_model(model_data$health_zone[[zone]], model, evaluation_steps)
      hz_results[zone] <- list(evo)
    } else{
      print(paste("Not enough days for", zone))
    }


 }
  results[[model_conf$desc]] <-list(
    national = national_evo,
    health_zone = hz_results
  )
}
names(results)

saveRDS(results, "results/latest.RDS")











   




