library(data.table)
library(rhxl)
library(EpiEstim)
library(scoringRules)
library(ggplot2)
source("evaluate.R")
source("util.R")
source("model.R")


models <- c(PoissonBranchingModel)
evaluation_steps = c(1, 7)#, 14, 21, 28)



data <- read_data("2019-07-04")
model_data <- prepare_data_model(data$national,
                                 data$health_zone)
results <- list()
for(model in models){
  national_evo <- run_model(model_data$national, model, evaluation_steps)
  hz_results <- list()
  for(zone in names(model_data$health_zone)){
    print(zone)
    if(length(model_data$health_zone[[zone]]$days) > 10){
      evo <- run_model(model_data$health_zone[[zone]], model, evaluation_steps)
      hz_results[zone] <- list(evo)
    } else{
      print(paste("Not enough days for", zone))
    }


  }
  results[class(evo$model)[1]] <-list(
    national = national_evo,
    health_zone = hz_results
  )

}










   




