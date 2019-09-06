source("util.R")


library(geojsonio)
library(data.table)
library(broom)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(xtable)
library(ISOweek)

plot_weekly_incidence <- function(data){
  #ggplot(hz) + geom_line(aes(x=report_date, y=confirmed_cases, color=health_zone))

  #ggplot(hz) + geom_column(aes(x=report_date, y=confirmed_cases, color=health_zone))

  hz = data$health_zone
  hz[, week:=ISOweek(report_date)]
  hz[, year:=year(report_date)]
  hz[, month:=month(report_date)]
  hz_by_week = hz[, .(inc=sum(confirmed_cases_change)), by=.(health_zone, week, year)]
  max_hz <- head(hz[, sum(confirmed_cases_change), by=health_zone][order(-V1)], 5)$health_zone
  hz_by_week[!(health_zone %in% max_hz), health_zone:="Other"]
  hz_by_week = hz_by_week[, .(inc=sum(inc)), by=.(health_zone, week, year)]
  hz_by_week = hz_by_week[, date:=ISOweek2date(paste(week, "1", sep="-"))]


  hz_by_week[inc<0, inc:=0]
  
  ggplot(hz_by_week) + geom_col(aes(x=date, y=inc, fill=health_zone), width=4) + xlab("Date") + ylab("New confirmed cases") + scale_fill_brewer("Health Zone", palette = "Dark2") + theme_bw()

}



create_map <- function(tot_data, column, legend_title){
  spdf <- geojson_read("data/geo/eastern_drc.geojson",  what = "sp")
  spdf_fortified <- tidy(spdf, region = "ADM2_NAME")

  tot_data$health_zone <- toupper(tot_data$health_zone)  
  spdf_fortified = spdf_fortified %>%
    left_join(. , tot_data, by=c("id"="health_zone"))

  spdf_fortified$cases[is.na(spdf_fortified$cases)] <- 0

  
  cnames <- aggregate(cbind(long, lat) ~ id,
                      data=spdf_fortified[spdf_fortified$cases > 0,],
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


plot_r <- function(r_object, start_day){

  setDT(r_object)
  r_object[, date:=start_day + T.Start]
  q <- ggplot(r_object) + geom_line(aes(x=date, y=get("Mean(R)"))) +
    geom_ribbon(aes(x=date, ymin=get("Quantile.0.025(R)"), ymax=get("Quantile.0.975(R)")), alpha=0.3) +
    theme_bw() + xlab("Date") + ylab("Reproduction Number")

}



create_score_table_national <- function(results){
  table <- data.frame(row.names="Model",
                      "Days ahead"=numeric(),
                      Model=character(),
                      Callibration=numeric(),
                      Sharpness=numeric(),
                      Bias=numeric(),
                      "Log score"=numeric(),
                      "Crps score"=numeric())
  
  for(model in names(results)){
    national <- results[[model]]$national$evaluation
    for(day in names(national)){
      row=data.frame(Model=model,
                     "Days ahead"=day,
                     Callibration=national[[day]]$pit_test_uni[["p.value"]],
                     Sharpness=mean(national[[day]]$sharpness),
                     Bias=mean(national[[day]]$bias),
                     "Log score"=mean(national[[day]]$log_score),
                     "Crps score"=mean(national[[day]]$crps_score)
                     )
      table <- rbind(table,row)
    }

  }
  return(table)
}

#prediction = results[["Bsts + poisson "]]$national$prediction
#inc = results[["Bsts + poisson "]]$national$model$incidence
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






create_score_table_by_zone <- function(results, model){
  table <- data.frame(row.names="Health.Zone",
                      "Days ahead" = numeric(),
                      Health.Zone=character(),
                      Callibration=numeric(),
                      Sharpness=numeric(),
                      Bias=numeric(),
                      "Log score"=numeric(),
                      "Crps score"=numeric())
  hz <- results[[model]]$health_zone
  for(health_zone in names(hz)){
    
    d <- results[[model]]$health_zone[[health_zone]]$evaluation
    for(day in names(d)){
      if("sharpness" %in% names(d[[day]])){
        row=data.frame(Health.Zone=health_zone,
                       "Days ahead"=day,
                       Callibration=d[[day]]$pit_test_uni[["p.value"]],
                       Sharpness=mean(d[[day]]$sharpness),
                       Bias=mean(d[[day]]$bias),
                       "Log score"=mean(d[[day]]$log_score),
                       "Crps score"=mean(d[[day]]$crps_score)
                       )
        table <- rbind(table,row)
        }
    }

  }
  return(xtable(table))
}

#create_score_table_by_zone(results, day, "Basic model")

create_score_table_regional <- function(results, days){
  table <- data.frame(row.names="Model",
                      "Days ahead"=character(),
                      Model=character(),
                      Sharpness=numeric(),
                      Bias=numeric(),
                      "Log score"=numeric(),
                      "Crps score"=numeric())
  for(model in names(results)){
    hz <- results[[model]]$health_zone
    sharpness <- c()
    bias <- c()
    log_score <- c()
    crps_score <- c()

    for(day in names(hz[[names(hz)[1]]]$evaluation)){
      for(health_zone in names(hz)){
        if("sharpness" %in% names(hz[[health_zone]]$evaluation[[day]])){
          sharpness <- c(sharpness, mean(hz[[health_zone]]$evaluation[[day]]$sharpness))
          bias <- c(sharpness, mean(hz[[health_zone]]$evaluation[[day]]$bias))
          log_score <- c(sharpness, mean(hz[[health_zone]]$evaluation[[day]]$log_score))
          crps_score <- c(sharpness, mean(hz[[health_zone]]$evaluation[[day]]$crps_score))
        }
      }
      row=data.frame(Model=model,
                     "Days ahead" = day,
                     Sharpness=mean(sharpness),
                     Bias=mean(bias),
                     "Log score"=mean(log_score),
                     "Crps score"=mean(crps_score)
                     )
      table <- rbind(table,row)
    }
  }
  return(table)
}



plot_scores <- function(scores){

  for(col in c("calibration", "bias", "sharpness", "centrality", "crps", "dss")){
    q <- ggplot(scores) + geom_line(aes(x=day, y=get(col), color=model)) +
      scale_color_brewer("Models", palette = "Dark2") + theme_bw() + ylab(col) + xlab("Forecasting Horizon") 
    ggsave(glue::glue("output/{col}.png"), width=7, heigh=3)
  }
}

#create_score_table_regional(results, day)

                               
