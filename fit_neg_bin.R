library(curry)
library(stats)
source("evaluate.R")
source("util.R")
source("model.R")

data <- read_data("2019-09-01")
model_data <- prepare_data_model(data$national,
                                 data$health_zone)



target <- function(size){
  ncnb <- purrr::partial(new_cases_neg_binom, size=size)
  
  model_conf <-list(
    desc="Negbin latest ",
    new_cases=ncnb,
    R_func=R_latest_value)
  model <- fit_model(model_data$national, model_conf$desc, model_conf$new_cases, model_conf$R_func)
  
  x <- day_ahead_prediction(model, start_day=16, days_ahead=2, N=4000) %>% filter(day == 1)
  crps_scores <- crps_sample(x$value, as.matrix(x[paste("V", 1:4000, sep="")]))
  return(mean(crps_scores))

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
