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
    desc="poisson_bsts",
    new_cases=new_cases_poisson,
    R_func=R_semilocal),
  list(
    desc="poisson_latest",
    new_cases=new_cases_poisson,
    R_func=R_latest_value),
  list(
    desc="nbin_bsts",
    new_cases=new_cases_neg_binom,
    R_func=R_semilocal),
  list(
    desc="nbin_latest",
    new_cases=new_cases_neg_binom,
    R_func=R_latest_value)

)
max_predict <- 28





data <- read_data("2019-09-01")
model_data <- prepare_data_model(data$national,
                                 data$health_zone)

runs <- list()


i <- 1

for (model_conf in models){
  print(model_conf$desc)
  model <- fit_model(model_data$national, model_conf$desc, model_conf$new_cases, model_conf$R_func)

  # national run
  runs[[i]] <- list(data=model_data$national,
                    model_name=model_conf$desc,
                    location="national",
                    model=model,
                    max_predict=max_predict)
  i <- i + 1
  
  for(zone in names(model_data$health_zone)){
    print(zone)
    if(length(model_data$health_zone[[zone]]$days) > 10){
      
      model <- fit_model(model_data$health_zone[[zone]], model_conf$desc,
                         model_conf$new_cases, model_conf$R_func)

      runs[[i]] <-list(data=model_data$health_zone[[zone]],
                       model=model,
                       model_name=model_conf$desc,
                       location=zone,
                       max_predict=max_predict)
      
    } else{
      print(paste("Not enough days for", zone))
    }
    i <- i + 1

  }
  print("end")
}


#output <- rbindlist(parallel::mclapply(runs, run_model, mc.cores=4))

#saveRDS(output, "results/latest.RDS")

output <- readRDS("results/latest.RDS")

combined_national <- output %>%
  filter(location != "national" & location != "national_combined") %>%
  group_by(model, day, start_day) %>%
  summarize_at(vars(starts_with("V", ignore.case=FALSE)), sum) %>% ungroup() %>%
  mutate(location = "national_combined") %>%
  inner_join(output %>% filter(location == "national") %>% dplyr::select(model, start_day, day, value),
             by=c("model"="model", "start_day"="start_day","day"="day")) 
 
output <- data.table::rbindlist(list(output %>% filter(location != "national_combined"), combined_national), fill=TRUE)

saveRDS(output, "results/latest.RDS")

output <- readRDS("results/latest.RDS")
scores <- evaluate(output, cores=4)

saveRDS(scores, "results/scores.RDS")




