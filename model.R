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
    incidence=NULL,
    serial_dist=NULL,
    Mean.SI=NULL,
    Std.SI=NULL,
    R=NULL,
    serial=NULL,
    new_cases=NULL,
    R_func=NULL,
    desc=NULL,
    initialize = function(days, incidence,
                          desc, new_cases, R_func,
                          Mean.SI=15.3, Std.SI=9.3){
      self$days <- days
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
      x <- EstimateR(I=incidence, T.Start=1:(length(incidence)-7),
                     T.End=8:length(incidence), Mean.SI=self$Mean.SI, Std.SI=self$Std.SI,
                     method="ParametricSI")
      self$R <- x$R
      self$serial<- x$SIDistr
      
    },
    predict_old=function(predict_days, N=1000){
      first_day <- predict_days[1] - 1
      if(predict_days[1] < 11){
        stop("First prediction day should be at least 10")
      }

      serial_interval <- self$serial["w[k]"]
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

      serial_interval <- self$serial[["w[k]"]]
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
          print(new)
          print(R)
          print(FI)
          print(self$R)
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
new_cases_neg_binom <- function(N, FI, self){
  return(rnbinom(N, mu=FI, size=0.53))
}


R_latest_value <- function(days, self, N=1){


  last_day <- days[1] - 8 # The -8 row corresponds to the period that ends on the day before the first prediction day
  print(last_day)
  mean <-  self$R["Mean(R)"][[1]][last_day]
  sd <-  self$R["Std(R)"][[1]][last_day]
  scale <- sd^2/mean
  shape <- mean^2/sd^2
  values <- rgamma(N * length(days), shape, scale=scale)
  dim(values) <- c(length(days), N)
  return(values)
}


R_semilocal <- function(days, self, N=1){

  last_day_before <- days[1] - 1
  mean_r <-  self$R["Mean(R)"][[1]][9:last_day_before]
  log_r <- log(mean_r)
  ss <- AddSemilocalLinearTrend(list(), log_r)
  model <- bsts(lr2, state.specification = ss, niter = N + 100)

  predicted <- predict(model, horizon=length(days), burn=100)$distribution

  return(exp(t(predicted)))
}
