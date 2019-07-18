library(data.table)
library(rhxl)
library(EpiEstim)
library(scoringRules)

source("evaluate.R")

days <- 1:500

incidence <- rpois(length(days), 5)

incidence2 <- rnbinom(length(days), 5, mu=5)

sm_good <- SimpleModel$new(days, incidence)

source("evaluate.R")
out <- day_ahead_prediction(sm_good)

x_good <- PIT(out)

sm_bad <- SimpleModel$new(days, incidence2)

x_bad <- PIT(sm_bad, days_ahead=1)


print("Calibration")
print(ks.test(x_good, "punif"))
print(ks.test(x_bad, "punif"))

print("Sharpness")
print(sharpness_madn(sm_good))
print(sharpness_madn(sm_bad))

print("bias")
print(bias(sm_good))
print(bias(sm_bad))


national <- as_hxl(read.csv("data/raw/national_2019-07-04.csv"))
setDT(national)
               
days <- 1:nrow(national)
incidence <- rev(national[,total_cases_change])

incidence[2] <- 0


source("model.R")
model <- PoissonBranchingModel$new(days, incidence, "gamma")

model$predict(nrow(national):(nrow(national)+10))



pits<-PIT(x)
print(test_uniform(pits))

logs_sample(x$values, x$predictions)
crps_sample(x$values, x$predictions)


## x = EstimateR(I=incidence, T.Start=3:(length(incidence)-7), T.End=10:length(incidence), Mean.SI=15.3, Std.SI=9.3, method="ParametricSI")

library(ggplot2)

ggplot() + geom_histogram(aes(x))
