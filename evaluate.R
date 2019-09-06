library(goftest)
library(dplyr)
library(tie)
library(tidyr)
library(scoringRules)
prob <- function(dist, value){
  if(value == -1){
    return(0)
  }
  return(sum(dist == value) / length(dist))
}


PIT <- function(data, N=10){
  p_values <- c()
  centralities <- c()
  predictions <- as.matrix(data[paste("V", 1:1000, sep="")])
  values <- data %>% pull(value)
  for( j in 1:N){
    us <- as.numeric(length(values))
    for(i in 1:length(values)){
      prediction <- predictions[i,]
      value <- values[i]

      current_ecdf <- ecdf(prediction)
      
      P_x <- current_ecdf(value)
      if(value == 0){
        P_x_m_1 = 0
      } else {
        P_x_m_1 <- current_ecdf(value - 1)
      }
      u <- P_x_m_1 + runif(1, 0,1) * (P_x - P_x_m_1)
      us[i] <- u
    }
    p_values <- c(p_values, ad.test(us)$p.value)
    centrality <- sum( us >= 0.25 & us < 0.75) / length(us)
    centralities <- c(centralities, centrality)

  }
  return(c(mean(centralities),
              mean(p_values)))
}




daily_score <- function(data){
  predictions <- as.matrix(data[paste("V", 1:1000, sep="")])
  values <- data %>% pull(value)

  return( c(values,
            sharpness_madn(predictions),
            bias(values, predictions),
            crps_sample(values, predictions),
            
            dss_sample(values, predictions))
         )

            

}




evaluate <- function(data){

  evaluate_PIT_day <- function(x_day){
    return(data %>% filter(!is.na(value), day==x_day) %>%
      group_by(model, day, location) %>% 
      do( bow(., tie(centrality, calibration) := PIT(.))))
    

  }
  
  PIT_results <- rbindlist(parallel::mclapply(unique(data[, day]), evaluate_PIT_day, mc.cores=4))

  evaluate_scores_day <- function(x_day){
    return(data %>% filter(!is.na(value) & day==x_day) %>%
     group_by(start_day, day, model, location) %>% 
    do( bow(., tie(value,sharpness,bias,crps,dss):=daily_score(.))))
  }
    
  by_start_day <- rbindlist(parallel::mclapply(unique(data[, day]), evaluate_scores_day, mc.cores=4))
  total <- by_start_day %>% group_by(model, location, day) %>%
    summarize(sharpness=mean(sharpness),
              bias=mean(bias),
              crps=mean(crps),
              dss=mean(dss))
  
  
  
  overall_results <- inner_join(total, PIT_results, by=c("model", "location", "day"))

  return(list("by_day"= by_start_day, "overall"= overall_results))
  
}

test_uniform <- function(data){
  return(ad.test(data, null="punif"))

}



sharpness_madn <- function(predictions){
  return(1/0.625 *median(abs(predictions - median(predictions))))
  
}


bias <- function(value, predictions){
  return(1 - (ecdf(predictions)(value) + ecdf(predictions)(value - 1)))
}

day_ahead_prediction <- function(model, start_day=16, days_ahead=1, N=1000){
  pred <- matrix(ncol=N)
  values <- c()
  results <- data.table()
  for( i in start_day:length(model$days)){
    end <- i + days_ahead -1
    p <- model$predict(i:end, N=N)
    ## if(class(p)[1] == "matrix"){
    ##   p <- p[nrow(p), ]
    ## }
    current_results <- data.table(p)
    current_results[, value:= model$incidence[i:end]]
    current_results[, day:=1:days_ahead]
    current_results[, dates:=model$dates[i:end]]
    current_results[, start_day:=i]
    current_results[, start_date:=model$dates[i - 1]]
    results <- rbind(results, current_results)
  }

  return(results)
}


