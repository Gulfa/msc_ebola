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
  setDT(hz)
  hz[, week:=ISOweek(report_date)]
  hz[, year:=year(report_date)]
  hz[, month:=month(report_date)]
  hz_by_week = hz[, .(inc=sum(confirmed_cases_change)), by=.(health_zone, week, year)]
  max_hz <- head(hz[, sum(confirmed_cases_change), by=health_zone][order(-V1)], 5)$health_zone
  hz_by_week[!(health_zone %in% max_hz), health_zone:="Other"]
  hz_by_week = hz_by_week[, .(inc=sum(inc)), by=.(health_zone, week, year)]
  hz_by_week = hz_by_week[, date:=ISOweek2date(paste(week, "1", sep="-"))]


  hz_by_week[inc<0, inc:=0]
  
  ggplot(hz_by_week) + geom_col(aes(x=date, y=inc, fill=health_zone), width=4) + xlab("Date") + ylab("New confirmed cases") + scale_fill_brewer("Health Zone", palette = "Dark2") + theme_bw() + theme(text = element_text(size=18), legend.position = "bottom")

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
    labs(fill=legend_title) + theme(text = element_text(size=18))
}


plot_r <- function(r_object, start_day){

  setDT(r_object)
  r_object[, date:=start_day + t_start]
  q <- ggplot(r_object) + geom_line(aes(x=date, y=get("Mean(R)"))) +
    geom_ribbon(aes(x=date, ymin=get("Quantile.0.025(R)"), ymax=get("Quantile.0.975(R)")), alpha=0.3) +
    theme_bw() + xlab("Date") + ylab("Reproduction Number")
  ggsave(glue::glue("output/nat_Rs.png"), width=8, height=4)
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



plot_scores <- function(scores, location){

  labels <- list(
    "calibration" = "Calibration",
    "bias" = "Bias",
    "sharpness" = "Sharpness",
    "centrality" = "Centrality",
    "crps"= "RPS",
    "dss" = "DSS"
    )
  for(col in c("calibration", "bias", "sharpness", "centrality", "crps", "dss")){
    
    
    q <- ggplot(scores) + geom_line(aes(x=day, y=get(col), color=model), size=1.3) +
      scale_color_brewer("Models", palette = "Dark2") + theme_bw() + ylab(labels[col]) + xlab("Forecasting Horizon") + theme(legend.position = "bottom", text = element_text(size=18), legend.text=element_text(size=12))
    ggsave(glue::glue("output/{location}_{col}.png"), width=8, height=4)
  }
}



plot_preds <- function(model_conf, data, x_location, results){
  model <- fit_model(data, model_conf$desc, model_conf$new_cases, model_conf$R_func)

  res <- results %>% filter(location == x_location & model==model_conf$desc)

  max_day = max(data$days)
  show_days <- c(50,100, 150, 200, 250, 300, 350, max_day)
  
  pred <- data.table(days=1:(max_day + 28),
                     pred=as.numeric(NA),
                     upper=as.numeric(NA),
                     lower=as.numeric(NA),
                     value=c(data$incidence, NA*numeric(length=28)))
  Rs <-  data.table(days=min(model$R[["t_end"]]):(max_day + 28),
                    pred=as.numeric(NA),
                    upper=as.numeric(NA),
                    lower=as.numeric(NA),
                    value=c(model$R[["Mean(R)"]], NA*numeric(length=28)))
  for(show_day in show_days){

    predictions <- res %>%  filter(start_day==show_day)
    if(nrow(predictions) == 0){
      next
    }
    predictions <- as.matrix(predictions %>% dplyr::select(starts_with("V", ignore.case = FALSE)))
    mean_I <- rowQuantiles(predictions, probs=0.5)
    quantiles <- rowQuantiles(predictions, probs=c(0.05, 0.95))
    pred[days %in% c(show_day:(show_day+27)), pred:=mean_I]
    pred[days %in% c(show_day:(show_day+27)), upper:=quantiles[, 2]]
    pred[days %in% c(show_day:(show_day+27)), lower:=quantiles[, 1]]
    
    R_pred <- R_semilocal(show_day:(show_day + 27), model, N=1000)
    mean_R <- rowQuantiles(R_pred, prob=0.5)
    quantiles <- rowQuantiles(R_pred, probs=c(0.05, 0.95))
    Rs[days %in% c(show_day:(show_day+27)), pred:=mean_R]
    Rs[days %in% c(show_day:(show_day+27)), upper:=quantiles[, 2]]
    Rs[days %in% c(show_day:(show_day+27)), lower:=quantiles[, 1]]
    
  }
  
  
  max <- 70
  pred[upper >= max, upper:=max]

  pred[, Date:=min(as.Date(data$dates)) + days]
  
  q <- ggplot(pred) + geom_line(aes(x=Date, y=value, color="Observed")) +
    geom_line(aes(x=Date, y=pred, color="prediction")) + theme_bw() + 
    geom_ribbon(aes(x=Date, ymin=lower, ymax=upper, group=1, fill="pred_unc"), alpha=0.4) +
    ylab("Incidence") +
    scale_color_manual("", labels=c("Observed Incidence", "Median prediction"),
                       values=c("black", "#d95f02")) + 
    scale_fill_manual("", labels=c("95% prediction interval"),
                       values=c("#d95f02")) +
    theme(legend.position = "bottom")
  
  ggsave(glue::glue("output/{x_location}_predictions.png"), q, width=7, height=3)
  
  max <- 15
  Rs[pred >= max, pred:=max]
  Rs[, Date:=min(as.Date(data$dates)) + days]
  q <- ggplot(Rs) + geom_line(aes(x=Date, y=value, color="r")) +
    geom_line(aes(x=Date, y=pred, color="prediction")) + theme_bw() + ylab("Reproduction Number") +
    geom_ribbon(aes(x=Date, ymin=lower, ymax=upper, group=1, fill="unc"), alpha=0.4) +
    scale_color_manual("", labels=c("r"="Estimated reproduction number", "prediction"="Median prediction"),
                       values=c("r"="black", "prediction"="#d95f02")) + 
    scale_fill_manual("", labels=c("95% prediction interval"),
                      values=c("#d95f02")) +
    theme(legend.position = "bottom")
  
  ggsave(glue::glue("output/{x_location}_Rs.png"), q, width=7, height=3) 
}
  


#create_score_table_regional(results, day)

                               
