library(EpiEstim)
library(bsts)
library(R6)

SimpleModel <-  R6Class(
  "SimpleModel",
  public=list(
    days = NULL,
    incidence = NULL,
    initialize = function(days, incidence){
      self$days <- days
      self$incidence <- incidence

    },
    predict = function(predict_days, N=1000){
      first_day <- predict_days[1] - 1
      mean_incidence <- mean(self$incidence[1:first_day])
      out <- rpois(N*length(predict_days), mean_incidence * matrix(1, length(predict_days),N))
      dim(out) <- c(length(predict_days), N)
      return(out)

     
    }
  )
)


BranchingModel <- R6Class(
  "PoissonBranchingModel",
  public=list(
    days =NULL,
    dates=NULL,
    incidence=NULL,
    serial_dist=NULL,
    Mean.SI=NULL,
    Std.SI=NULL,
    R=NULL,
    serial=NULL,
    new_cases=NULL,
    R_func=NULL,
    desc=NULL,
    initialize = function(days, dates, incidence,
                          desc, new_cases, R_func,
                          Mean.SI=15.3, Std.SI=9.3){
      self$days <- days
      self$dates <- dates
      self$incidence <- incidence
      self$Mean.SI <- Mean.SI
      self$Std.SI <- Std.SI
      self$new_cases <- new_cases
      self$R_func <- R_func
      self$desc <- desc
      self$fit()
      
    },
    fit=function(){
      incidence <- self$incidence
      x <- estimate_R(incidence, 
                      config = make_config(
                        t_start=2:(length(incidence)-7),
                        t_end=9:length(incidence), mean_si=self$Mean.SI,
                        std_si=self$Std.SI),
                      method="parametric_si")
      self$R <- x$R
      self$serial<- x$si_distr
      
    },
    predict_old=function(predict_days, N=1000){
      first_day <- predict_days[1] - 1
      if(predict_days[1] < 11){
        stop("First prediction day should be at least 10")
      }

      serial_interval <- self$serial
      incidences <- matrix(nrow=length(predict_days))
      for(i in 1:N){
        incidence <- self$incidence[1:first_day]
        for (day in predict_days){
          if (length(incidence) >= length(serial_interval)){
            FI <- sum(incidence[(length(incidence) - length(serial_interval)):length(incidence)]
                     * rev(serial_interval))
          }else{
            FI <- sum(incidence * rev(serial_interval[(length(serial_interval) - length(incidence)):length(serial_interval)]))
          }
          R <- self$R_func(day)
          incidence <- c(incidence, rpois(1, FI*R))
        }
        incidences <- cbind(incidences, incidence[(first_day+1):(first_day+length(predict_days))])
                            
      }
     return(incidences[,2:(N+1)])
    },


    
    predict=function(predict_days, N=1000){
      first_day <- predict_days[1] - 1
      if(predict_days[1] < 11){
        stop("First prediction day should be at least 10")
      }

      serial_interval <- self$serial
      incidences <- matrix(nrow=first_day + length(predict_days), ncol=N)
      incidences[1:first_day, ] = self$incidence[1:first_day]
      
      Rs <- self$R_func(predict_days, self, N)
      
      for (day in predict_days){
        
        if (day > length(serial_interval)){
          FI <- colSums(incidences[(day - length(serial_interval)):(day-1), ]
                        * rev(serial_interval))
        }else{
            FI <- colSums(incidences[1:(day-1),] * rev(serial_interval[1:(day-1)]))
        }
        R <- Rs[day - predict_days[1] + 1, ]
        new <- self$new_cases(N, FI*R, self)
        if(any(is.na(new))){
          print(day)
          print(predict_days)
          print(new)
          print(R)
          print(FI)
          print(max(R))
          print(max(R*FI))
          stop("NANs prodcued")
        }
        incidences[day,] <-new

      }
     return(incidences[predict_days,])
    }

  )

)



new_cases_poisson <- function(N, FI, self){
  return(rpois(N, FI))
}
new_cases_neg_binom <- function(N, FI, self, size=8){
  return(rnbinom(N, mu=FI, size=size))
}


R_latest_value <- function(days, self, N=1){


  last_day <- days[1]

  r_values <- self$R %>% filter(t_end == last_day)
  mean <-  r_values[["Mean(R)"]]
  sd <-  r_values[["Std(R)"]]
  scale <- sd^2/mean
  shape <- mean^2/sd^2
  values <- rgamma(N * length(days), shape, scale=scale)
  dim(values) <- c(length(days), N)
  return(values)
}


R_semilocal <- function(days, self, N=1){

  last_day_before <- days[1] - 1
  mean_r <-  self$R["Mean(R)"][[1]][9:last_day_before]

  b <- 15

  mean_r[mean_r >= b] <-  b - 0.1

  log_r <- log(mean_r / (b - mean_r))
  
  ss <- AddSemilocalLinearTrend(list(), log_r, slope.ar1.prior = NormalPrior(0, 0.1))
#                                level.sigma.prior = SdPrior(0.01, sample.size = 200))
  model <- bsts(log_r, state.specification = ss, niter = N + 100, ping=0)

  predicted <- predict(model, horizon=length(days), burn=100)$distribution

  return_value <- b*exp(predicted) / (1 + exp(predicted))
  return(t(return_value))
}



plot_r_pred <- function(predicted){
  mean_R <- rowMeans(predicted)
  quantiles <- rowQuantiles(predicted, probs=c(0.05, 0.95))
  days <- 1:nrow(predicted)
  q <- ggplot() +  geom_line(aes(x=days, y=mean_R)) +
    geom_ribbon(aes(x=days, ymin=quantiles[,1],
                    ymax=quantiles[,2]),alpha=0.3)


}
