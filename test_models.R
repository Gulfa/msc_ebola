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
    desc="Basic Neg Binom Model",
    new_cases=new_cases_neg_binom,
    R_func=R_latest_value)

)
evaluation_steps = c(1,7) # 14, 21, 28)



data <- read_data("2019-07-04")
model_data <- prepare_data_model(data$national,
                                 data$health_zone)
results <- list()
for(model_conf in models){
  print(model_conf$desc)
  model <- fit_model(model_data$national, model_conf$desc, model_conf$new_cases, model_conf$R_func)
   x <- day_ahead_prediction(model, start_day=16, days_ahead = 1, N=500) 
}

