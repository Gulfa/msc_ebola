library(rhxl)
library(data.table)

source("evaluate.R")



fix_data_national <- function(data){
  # Mistakes found in the data
  data[report_date == "2019-05-31", confirmed_cases:=1188]
  data[report_date == "2019-05-31", confirmed_cases_change:= 8]
  data[report_date == "2019-06-01", confirmed_cases_change:= 12]
  data[report_date == "2019-06-01", confirmed_cases_change:= 12]
  return(data)
}

fix_data_health_zone <- function(data){
  # Mistakes found in the data
  setDT(data)
  data[report_date == "2018-12-24" & health_zone=="Lubero", confirmed_cases:=0]
  return(data)
}




fix_missing_dates <- function(data, start_date=NULL){

  data <- data %>% mutate(report_date =as.Date(report_date))
  data <- data %>% arrange(report_date)
  setDT(data)
  data[confirmed_cases == 0 & is.na(confirmed_cases_change), confirmed_cases_change:=0]
  prev <- head(data, 1)
  if(is.na(prev$confirmed_cases_change)){
    data$confirmed_cases_change[1]=data$confirmed_cases[1]
  }
     
  to_add_df <- data.frame()
  if(!is.null(start_date) && min(data[, report_date]) > start_date){
    for(i in 0:(min(data[, report_date]) - start_date)){
      to_add_df <- rbind(to_add_df, data.table(
                                      report_date = start_date + i,
                                      confirmed_cases = 0,
                                      confirmed_cases_change = 0))
    }
    

  }
  for(i in 2:nrow(data)){
    if((data$report_date[i] - prev$report_date !=1)){
      N = as.integer((data$report_date[i] - prev$report_date)) - 1
      change <- data$confirmed_cases[i] - prev$confirmed_cases
      to_add <- floor(change/(N +1))
      for(j in 1:N){
        to_add_df <- rbind(to_add_df, data.table(
                         report_date = data$report_date[i] + j - N -1,
                         confirmed_cases = prev$confirmed_cases + to_add*j,
                         confirmed_cases_change = to_add))
      }
      data$confirmed_cases_change[i] <- change - to_add*N
      

    }
    prev <- data %>% slice(i)
    
  }
  if(nrow(to_add_df) > 0){
    data <- rbind(data, to_add_df, fill=TRUE) %>% arrange(report_date)
  }
  return(data)
  
}

read_data <- function(in_date){

  national <- as_hxl(read.csv(
    paste0("data/raw/national_", in_date, ".csv", sep="")
  ))
  setDT(national)
  national <- fix_data_national(national)
  national <- fix_missing_dates(national)
  setDT(national)
  start_date = min(national[,report_date])

  health_zone <- as_hxl(read.csv(
    paste0("data/raw/health_zone_", in_date, ".csv", sep="")
  ))
  health_zone <- fix_data_health_zone(health_zone)
  new_hz <- data.frame()
  for(x_health_zone in unique(health_zone$health_zone)){
    hz <- health_zone %>% filter(health_zone == x_health_zone)
    n_hz <- fix_missing_dates(hz, start_date=start_date)
    n_hz <- n_hz %>% mutate(health_zone = hz$health_zone[1], province=hz$province[1])
    new_hz <- rbind(new_hz, n_hz)
  }
  setDT(new_hz)

  
  return(list(national=national,
              health_zone=new_hz))

}

run_model <- function(run, N=1000){
  model <- run[["model"]]
  data <- data.table(data.frame(run[["data"]]))
  max_predict <- run[["max_predict"]]
  
  start_day <- max(18, min(data[incidence >0, days]))

  results <- data.table()
   
  results <- day_ahead_prediction(model, start_day=start_day, days_ahead = max_predict, N=N)
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
  incidence <- national[,confirmed_cases_change]
  dates <- rev(national[,report_date])
#  incidence[2] <- 0
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




