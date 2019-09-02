library(rhxl)
library(data.table)

source("evaluate.R")



fix_data_national <- function(data){
  # Mistakes found in the data
  data[report_date == "2019-05-31", confirmed_cases:=1188]
  data[report_date == "2019-05-31", confirmed_cases_change:= 8]
  data[report_date == "2019-06-01", confirmed_cases_change:= 12]
  return(data)
}


read_data <- function(in_date){

  national <- as_hxl(read.csv(
    paste0("data/raw/national_", in_date, ".csv", sep="")
  ))
  setDT(national)
  national <- fix_data_national(national)
  health_zone <- as_hxl(read.csv(
    paste0("data/raw/health_zone_", in_date, ".csv", sep="")
  ))
  setDT(health_zone)

  
  return(list(national=national,
              health_zone=health_zone))

}

run_model <- function(run){
  model <- run[["model"]]
  data <- data.table(data.frame(run[["data"]]))
  max_predict <- run[["max_predict"]]
  
  start_day <- max(16, min(data[incidence >0, days]))

  results <- data.table()
   
  results <- day_ahead_prediction(model, start_day=start_day, days_ahead = max_predict, N=1000)
  results[, model:=run[["model_name"]]]
  results[, location:=run[["location"]]]
  return(results)
}

fit_model <- function(data, desc, new_cases, R_func ){
  model <- BranchingModel$new(data$days, data$dates,
                              data$incidence,  desc, new_cases, R_func)
  return(model)
}

evaluate_model <- function(model, days_ahead=1){

  pits<-PIT(x)
  test_uni <- test_uniform(pits)
  sharpness <- sharpness_madn(x)
  bias_values <- bias(x)
  dss_scores <- dss_sample(x$values, x$predictions)
  crps_scores <- crps_sample(x$values, x$predictions)

  return(list(
    pit_test_uni=test_uni,
    pits=pits,
    sharpness=sharpness,
    bias=bias_values,
    dss_score=log_scores,
    crps_score=crps_scores))
}


prepare_data_model <- function(national, health_zone){
  national_days <- 1:nrow(national)
  incidence <- rev(national[,confirmed_cases_change])
  dates <- rev(national[,report_date])
  incidence[2] <- 0
  national_data <- list(days=national_days,
                        dates=dates,
                        incidence=incidence)
  health_zone_data <-  list()
  
  for(hs in unique(health_zone[, health_zone])){
    hs_d = health_zone[health_zone == hs]

    incidence = hs_d[, confirmed_cases_change]

    incidence[incidence < 0] = 0 # REMOVE NEGATIVE VALUES
    incidence[is.na(incidence)] = 0 # REMOVE NEGATIVE VALUES

    # This is maybe not so good, REVESIT
    if (sum(incidence) > 1){
    
      health_zone_data[[hs]] <- list(days=1:nrow(hs_d),
                                     dates=hs_d[, report_date],
                                     incidence=incidence)
    } else {
      print(paste("Skipped", hs, "since too few cases"))
    }
  }
  return(list(national=national_data,
              health_zone=health_zone_data))
}




