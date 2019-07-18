source("evaluate.R")
source("util.R")
source("model.R")


models = c(PoissonBranchingModel)
evaluation_steps = c(1, 7)#, 14, 21, 28)



data <- read_data("2019-07-04")
model_data <- prepare_data_model(data$national,
                                 data$health_zone)

m <- fit_model(model_data$national, PoissonBranchingModel)

m$predict(50:200,N=1000)
