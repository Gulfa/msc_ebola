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

run_model <- function(data, model, evaluation_steps){

  evaluations <- list()
  for (step in evaluation_steps){
    print(step)
    evaluations[paste0(c("days", step))] <- list(evaluate_model(model, days_ahead=step))
  }
  last_day <- model$days[length(model$days)]
  predict_days <- last_day:(last_day+28)
  predictions <- model$predict(predict_days, N=1000)
  results <- list(model=model,
                  evaluation=evaluations,
                  predictions=predictions
                )

  return(results)
}

fit_model <- function(data, desc, new_cases, R_func ){
  model <- BranchingModel$new(data$days, data$incidence, desc, new_cases, R_func)
  return(model)
}

evaluate_model <- function(model, days_ahead=1){
  x <- day_ahead_prediction(model, start_day=16, days_ahead = days_ahead, N=2000)
  pits<-PIT(x)
  test_uni <- test_uniform(pits)
  sharpness <- sharpness_madn(x)
  bias_values <- bias(x)
  log_scores <- logs_sample(x$values, x$predictions, bw=apply(x$predictions, 1, stats::bw.nrd0))
  crps_scores <- crps_sample(x$values, x$predictions)

  return(list(
    pit_test_uni=test_uni,
    pits=pits,
    sharpness=sharpness,
    bias=bias_values,
    log_score=log_scores,
    crps_score=crps_scores))
}


prepare_data_model <- function(national, health_zone){
  national_days <- 1:nrow(national)
  incidence <- rev(national[,confirmed_cases_change])
  incidence[2] <- 0
  national_data <- list(days=national_days, incidence=incidence)
  health_zone_data <-  list()
  
  for(hs in unique(health_zone[, health_zone])){
    hs_d = health_zone[health_zone == hs]

    incidence = hs_d[, confirmed_cases_change]

    incidence[incidence < 0] = 0 # REMOVE NEGATIVE VALUES
    incidence[is.na(incidence)] = 0 # REMOVE NEGATIVE VALUES


    # This is maybe not so good, REVESIT
    if (sum(incidence) > 3){
    
      health_zone_data[[hs]] <- list(days=1:nrow(hs_d),
                                     incidence=incidence)
    } else {
      print(paste("Skipped", hs, "since too few cases"))
    }
  }
  return(list(national=national_data,
              health_zone=health_zone_data))
}





plot_incidence <- function(data){
  ggplot(hz) + geom_line(aes(x=report_date, y=confirmed_cases, color=health_zone))

  ggplot(hz) + geom_column(aes(x=report_date, y=confirmed_cases, color=health_zone))

  hz = data$health_zone
  hz[, week:=week(report_date)]
  hz[, year:=year(report_date)]
  hz[, month:=month(report_date)]
  hz_by_week = hz[, .(inc=sum(confirmed_cases_change), date=min(report_date)), by=.(health_zone, week, year)]
  
  hz_by_month = hz[, .(inc=sum(confirmed_cases_change), date=min(report_date)), by=.(health_zone, month, year)]
  
  
  ggplot(hz) + geom_line(aes(x=report_date, y=confirmed_cases, color=health_zone))
  ggplot(hz_by_week) + geom_col(aes(x=date, y=inc, fill=health_zone))
  ggplot(hz_by_month) + geom_col(aes(x=date, y=inc, fill=health_zone))

}
