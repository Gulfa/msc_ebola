library(EpiEstim)
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


PoissonBranchingModel <- R6Class(
  "PoissonBranchingModel",
  public=list(
    days =NULL,
    incidence=NULL,
    serial_dist=NULL,
    Mean.SI=NULL,
    Std.SI=NULL,
    R=NULL,
    serial=NULL,
    initialize = function(days, incidence, Mean.SI=15.3, Std.SI=9.3){
      self$days <- days
      self$incidence <- incidence
      self$Mean.SI <- Mean.SI
      self$Std.SI <- Std.SI
      self$fit()
    },
    fit=function(){
      incidence <- self$incidence
      x <- EstimateR(I=incidence, T.Start=5:(length(incidence)-7),
                     T.End=12:length(incidence), Mean.SI=self$Mean.SI, Std.SI=self$Std.SI,
                     method="ParametricSI")
      self$R <- x$R
      self$serial<- x$SIDistr
      
    },
    R_func=function(day, N=1){
      mean <-  self$R["Mean(R)"][[1]][nrow(self$R)]
      sd <-  self$R["Std(R)"][[1]][nrow(self$R)]
      scale <- sd^2/mean
      shape <- mean^2/sd^2
      return(rgamma(N, shape, scale=scale))
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
      for (day in predict_days){
        if (day > length(serial_interval)){
          FI <- colSums(incidences[(day - length(serial_interval)):(day-1), ]
                        * rev(serial_interval))
        }else{
            FI <- colSums(incidences[1:(day-1),] * rev(serial_interval[1:(day-1)]))
        }
        R <- self$R_func(day, N)
        new <- rpois(N, FI*R)
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
