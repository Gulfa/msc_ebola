library(curry)
library(stats)
source("evaluate.R")
source("util.R")
source("model.R")

data <- read_data("2019-07-04")
model_data <- prepare_data_model(data$national,
                                 data$health_zone)



target <- function(size){
  ncnb <- partial(new_cases_neg_binom, list(size=size))
  
  model_conf <-list(
    desc="Bsts + poisson ",
    new_cases=ncnb,
    R_func=R_latest_value)
  model <- fit_model(model_data$national, model_conf$desc, model_conf$new_cases, model_conf$R_func)
  
  x <- day_ahead_prediction(model, start_day=16, days_ahead=1, N=10000)
  log_scores <- logs_sample(x$values, x$predictions, bw=apply(x$predictions, 1, stats::bw.nrd0))
  return(mean(log_scores))

}

target(9)

optim(0.53, target)

target(27)

v = 1:20
s = c()
for(i in v){
  s = c(s, target(i))
}

plot(v, s)
