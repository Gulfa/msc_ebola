source("util.R")


library(geojsonio)
library(data.table)
library(broom)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(xtable)


create_map <- function(data, column, legend_title){
  spdf <- geojson_read("data/geo/eastern_drc.geojson",  what = "sp")
  spdf_fortified <- tidy(spdf, region = "ADM2_NAME")

  data$health_zone <- toupper(data$health_zone)  
  spdf_fortified = spdf_fortified %>%
    left_join(. , data, by=c("id"="health_zone"))

  spdf_fortified$cases[is.na(spdf_fortified$cases)] <- 0


  cnames <- aggregate(cbind(long, lat) ~ id,
                      data=spdf_fortified,
                      FUN=function(x)mean(range(x)))
  
  
  
  q <- ggplot() +
    geom_polygon(data = spdf_fortified, aes(fill = get(column), x = long, y = lat,
                                          group = group), colour="grey") +
    theme_void() + 
    coord_map() +
    scale_fill_gradient(low = '#fff7ec', high = '#7f0000') +
    geom_label_repel(data=cnames, aes(long, lat, label = id), size=3) +
    labs(fill=legend_title)
}




data <- read_data("2019-07-04")
hz <- data$health_zone
data <- hz[, .(cases = sum(total_cases)), by=health_zone]
q <- create_map(data, "cases", "Total Cases")


results <- readRDS("results/latest.RDS")

day <- "1"


create_score_table_national <- function(results, day){
  table <- data.frame(row.names="Model",
                      Model=character(),
                      Callibration=numeric(),
                      Sharpness=numeric(),
                      Bias=numeric(),
                      "log score"=numeric(),
                      "crps score"=numeric())
  for(model in names(results)){
    national <- results[[model]]$national$evaluation
    row=data.frame(Model=model, 
          Callibration=national[[day]]$pit_test_uni[["p.value"]],
          Sharpness=mean(national[[day]]$sharpness),
          Bias=mean(national[[day]]$bias),
          "log score"=mean(national[[day]]$log_score),
          "crps score"=mean(national[[day]]$crps_score)
          )
    print(row)
    table <- rbind(table,row)
  }
  return(xtable(table))
}


prediction = results[["Bsts + poisson "]]$national$prediction
inc = results[["Bsts + poisson "]]$national$model$incidence
plot_prediction <- function(inc, prediction){

  hist_days = 1:length(inc)
  cum_incidence = cumsum(inc)
  
  days = hist_days[length(hist_days)] + 1:nrow(prediction)
  c_pred = cum_incidence[length(cum_incidence)] + apply(prediction, 2, cumsum)
  
  mean_I <- rowMeans(c_pred)
  quantiles <- rowQuantiles(c_pred, probs=c(0.05, 0.95))

  q <- ggplot() + geom_line(aes(x=hist_days, y=cum_incidence)) +
    geom_line(aes(x=days, y=mean_I)) + geom_ribbon(aes(x=days, ymin=quantiles[,1],
                                                       ymax=quantiles[,2]),alpha=0.3)
  
}




create_score_table_national(results, day)

create_score_table_by_zone <- function(results, day, model){
  table <- data.frame(row.names="Health.Zone",
                      Health.Zone=character(),
                      Callibration=numeric(),
                      Sharpness=numeric(),
                      Bias=numeric(),
                      "log score"=numeric(),
                      "crps score"=numeric())
  hz <- results[[model]]$health_zone
  for(health_zone in names(hz)){
    d <- results[[model]]$health_zone[[health_zone]]$evaluation[[day]]
     row=data.frame(Health.Zone=health_zone, 
          Callibration=d$pit_test_uni[["p.value"]],
          Sharpness=mean(d$sharpness),
          Bias=mean(d$bias),
          "log score"=mean(d$log_score),
          "crps score"=mean(d$crps_score)
          )
    table <- rbind(table,row)
  }
  return(xtable(table))
}

create_score_table_by_zone(results, day, "Basic model")

create_score_table_regional <- function(results, day){
  table <- data.frame(row.names="Model",
                      Model=character(),
                      Sharpness=numeric(),
                      Bias=numeric(),
                      "log score"=numeric(),
                      "crps score"=numeric())
  for(model in names(results)){
    hz <- results[[model]]$health_zone
    sharpness <- c()
    bias <- c()
    log_score <- c()
    crps_score <- c()

    for(health_zone in names(hz)){
      sharpness <- c(sharpness, mean(hz[[health_zone]]$evaluation[[day]]$sharpness))
      bias <- c(sharpness, mean(hz[[health_zone]]$evaluation[[day]]$bias))
      log_score <- c(sharpness, mean(hz[[health_zone]]$evaluation[[day]]$log_score))
      crps_score <- c(sharpness, mean(hz[[health_zone]]$evaluation[[day]]$crps_score))

    }
    print(log_score)
    row=data.frame(Model=model, 
          Sharpness=mean(sharpness),
          Bias=mean(bias),
          "log score"=mean(log_score),
          "crps score"=mean(crps_score)
          )
    table <- rbind(table,row)
  }
  return(xtable(table))
}


create_score_table_regional(results, day)

                               
