library(goftest)
library(dplyr)
library(tie)
library(tidyr)
library(scoringRules)
library(matrixStats)
prob <- function(dist, value){
  if(value == -1){
    return(0)
  }
  return(sum(dist == value) / length(dist))
}


PIT <- function(data, N=30){
  p_values <- c()
  centralities <- c()
  predictions <- as.matrix(data %>% dplyr::select(starts_with("V", ignore.case=FALSE)))
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
    centrality <- sum( us >= 0.25 & us < 0.75) / length(us) - 0.5
    centralities <- c(centralities, centrality)

  }
  return(c(mean(centralities),
              mean(p_values)))
}




daily_score <- function(data){
  predictions <- as.matrix(data %>% dplyr::select(starts_with("V", ignore.case=FALSE)))
  values <- data %>% pull(value)
  return(list(
           sharpness_madn(predictions),
           bias(values, predictions),
           crps_sample(values, predictions),
           dss_sample(values, predictions))
         )

            

}




evaluate <- function(data, cores=1){
  
  setDT(data)
 evaluate_PIT_day <- function(x_day){
    return(data %>% filter(!is.na(value), day==x_day) %>%
      group_by(model, day, location) %>%
      do( bow(., tie(centrality, calibration) := PIT(.))))


  }

  evaluate_PIT_combined_hz_day <- function(x_day){
    return(data %>% filter(!is.na(value) & day==x_day & location != "national" &
                           location != "national_combined") %>%
      group_by(model, day) %>%
      do( bow(., tie(centrality, calibration) := PIT(.))))


  }


  d <- data %>% filter(!is.na(value))
  scores <- daily_score(d)
  by_start_day <- d %>% mutate(sharpness=scores[[1]],
                    bias=scores[[2]],
                    crps=scores[[3]],
                    dss=scores[[4]]
                    )
  print("Starting PIT")
  PIT_results <- rbindlist(parallel::mclapply(unique(data[, day]), evaluate_PIT_day, mc.cores=cores))

  PIT_results_combined_hz <- rbindlist(parallel::mclapply(unique(data[, day]), evaluate_PIT_combined_hz_day, mc.cores=cores))

  PIT_results_combined_hz[, location:="hz_combined"]

  PIT_results <- rbindlist(list(PIT_results, PIT_results_combined_hz), use.names=TRUE)

  total <- by_start_day %>% group_by(model, location, day) %>%
    summarize(sharpness=mean(sharpness),
              bias=mean(bias),
              crps=mean(crps),
              dss=mean(dss, na.rm=T))

  total_combined_hz <- total %>% filter(location != "national" & location != "national_combined") %>%
    group_by(model, day) %>% summarize(sharpness=mean(sharpness),
                                       bias=mean(bias),
                                       crps=mean(crps),
                                       dss=mean(dss, na.rm=T)) %>%
                                           mutate(location ="hz_combined")

  total <- rbindlist(list(total, total_combined_hz), use.names=TRUE)

  overall_results <- inner_join(total, PIT_results, by=c("model", "location", "day"))

  return(list("by_day"= by_start_day, "overall"= overall_results))
  
}

test_uniform <- function(data){
  return(ad.test(data, null="punif"))

}



sharpness_madn <- function(predictions){
  return(1/0.625 *rowMedians(abs(predictions - rowMedians(predictions))))
  
}

bias_helper <- function(ecdf_function, value){
  return(1 - (ecdf_function(value) + ecdf_function(value - 1)))
}
  


bias <- function(values, predictions){
  return(mapply(
    bias_helper,
    apply(predictions, 1, ecdf),
    values
    )
  )
  
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


