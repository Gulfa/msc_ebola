library(goftest)

cum_prob <- function(dist, value){
  if(value == -1){
    return(0)
  }
  return(sum(dist <= value) / length(dist))
}


PIT <- function(predictions){
  us <- c()
  for( i in 1:length(predictions$values)){
    prediction <- predictions$predictions[i,]
    value <- predictions$values[i]
    P_x <- cum_prob(prediction, value)
    if(value == 0){
      P_x_m_1 = 0
    } else {
      P_x_m_1 <- cum_prob(prediction, value - 1)
    }
    u <- P_x_m_1 + runif(1, 0,1) * (P_x - P_x_m_1)
    us <- c(us, u)
  }
  return(us)
}


test_uniform <- function(data){
  return(ad.test(data, null="punif"))

}



sharpness_madn <- function(predictions){
  S <- c()
  for( i in 1:length(predictions$values)){
    prediction <- predictions$predictions[i,]
    S <- c(S, 1/0.625 *median(abs(prediction - median(prediction))))
   }
  return(S)
}


bias <- function(predictions){
  b <- c()
  for( i in 1:length(predictions$values)){
    prediction <- predictions$predictions[i,]
    value <- predictions$values[i]
    P_x <- cum_prob(prediction, value)
    b <- c(b, 1 - (cum_prob(prediction, value) + cum_prob(prediction, value - 1)))
  }
  return(b)

}

day_ahead_prediction <- function(model, start_day=1, days_ahead=1, N=1000){
  pred <- matrix(ncol=N)
  values <- c()
  for( i in model$days[(start_day + days_ahead): length(model$days)]){
    p <- model$predict((i-days_ahead +1):i, N=N)
    if(class(p)[1] == "matrix"){
      p <- p[nrow(p), ]
    }
    pred <- rbind(pred, p)
    values <-c(values,  model$incidence[i])
  }

  return(list(predictions=pred[2:nrow(pred),], values=values))
}


score<- function(model, scoring_function, days_ahead=1, start_day=1){
  us <- c()
  for( i in model$days[(start_day + days_ahead): length(model$days)]){
    prediction <- model$predict(c(i))
    value <- model$incidence[i]
    P_x <- cum_prob(prediction, value)
    if(value == 0){
      P_x_m_1 = 0
    } else {
      P_x_m_1 <- cum_prob(prediction, value - 1)
    }
    u <- P_x_m_1 + runif(1, 0,1) * (P_x - P_x_m_1)
    us <- c(us, u)
  }
  return(us)
}
