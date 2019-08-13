source("evaluate.R")
source("util.R")
source("model.R")


model_conf <-list(
    desc="Bsts + poisson ",
    new_cases=new_cases_poisson,
    R_func=R_semilocal)


evaluation_steps = c(1,7) # 14, 21, 28)



data <- read_data("2019-07-04")
model_data <- prepare_data_model(data$national,
                                 data$health_zone)
model <- fit_model(model_data$national, model_conf$desc, model_conf$new_cases, model_conf$R_func)

x <- day_ahead_prediction(model, start_day=16, days_ahead =28)

last_day_before <- 300
mean_r <-  self$R["Mean(R)"][[1]][9:last_day_before]
log_r <- log(mean_r)

a <- 0

b <- 12

log_r2 <- log(mean_r/(b - mean_r))



N= 1000

ss <- AddSemilocalLinearTrend(list(), log_r, slope.ar1.prior = NormalPrior(0, 0.1))
model1 <- bsts(log_r, state.specification = ss, niter = N + 100, ping=0)





ss2 <- AddSemilocalLinearTrend(list(), log_r2, slope.ar1.prior = NormalPrior(0, 0.1))
model2 <- bsts(log_r2, state.specification = ss2, niter = N + 100, ping=0)

predicted1 <- predict(model1, horizon=100, burn=100)

real_scale_1 <- exp(predicted1$distribution)



predicted2 <- predict(model2, horizon=100, burn=100)

real_scale_2 <- b*exp(predicted2$distribution)/(1 + exp(predicted2$distribution))




quantiles <- colQuantiles(real_scale_1, probs=c(0.01, 0.99))

ggplot() +  geom_line(aes(x=301:400, y=colQuantiles(real_scale_1, probs=0.5))) +
     geom_line(aes(x=9:300, y=mean_r)) + geom_ribbon(aes(x=301:400, ymin=quantiles[,1],
                                                       ymax=quantiles[,2]),alpha=0.3)
  
