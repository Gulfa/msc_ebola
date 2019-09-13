source("plots.R")
source("util.R")
source("model.R")
library(dplyr)
library(ggplot2)
library(xtable)
library(matrixStats)

data <- read_data("2019-09-01")
model_data <- prepare_data_model(data$national,
                                 data$health_zone)


hz <- data$health_zone

q <- plot_weekly_incidence(data)

ggsave("output/epi_curve.png", q, width = 9, height=6)

tot_data <- hz[, .(cases = max(confirmed_cases)), by=health_zone]

q <- create_map(tot_data, "cases", "Total Cases")
ggsave("output/tot_map.png", q, width=7, height=7)


mu <- 10
x <- 0:40
df <- data.frame(
  x=x,
  Poisson=dpois(x, lambda=mu),
  "Binomial9"=dnbinom(x, mu=mu, size=9),
  "Binomial3"=dnbinom(x, mu=mu, size=3),
  "Binomial1"=dnbinom(x, mu=mu, size=1),
  "Binomial5"=dnbinom(x, mu=mu, size=0.5)
) %>% gather("Distribution", "value", -x) %>% mutate(
                                                "Distribution" = recode(Distribution,
                                                                        "Binomial9" = "NegBin(k=9)",
                                                                        "Binomial3" = "NegBin(k=3)",
                                                                        "Binomial1" = "NegBin(k=1)",
                                                                        "Binomial5" = "NegBin(k=0.5)",
                                                                        
                                                                        ))

q <- ggplot(df) + geom_line(aes(x=x, y=value, color=Distribution)) + scale_color_brewer("Models", palette = "Dark2") + theme_bw() +xlab("New Cases") + ylab("Probability mass")
ggsave("output/prob_dist.png", q, width=7, height=5)
  




results <- readRDS("results/latest.RDS")

scores <- readRDS("results/scores.RDS")

overall_scores <- ungroup(scores$overall) %>% mutate(model=recode(model,
                                                "poisson_bsts"="Poisson Semilocal",
                                                "nbin_bsts"="NegBin Semilocal",
                                                "poisson_latest"="Poisson Latest",
                                                "nbin_latest"="NegBin Latest"))

#q <- plot_r(results[["Basic model"]]$national$model$R, min(hz[, report_date]))

#ggsave("output/nat_rep.png", q, width = 9, height=6)


plot_scores(overall_scores %>% filter(location=="national"), location="national")
plot_scores(overall_scores %>% filter(location=="national_combined"), location="national_cobmined")


table <- overall_scores %>% filter(location == "national" & day %in% c(1,7,14,21,28))%>%
  arrange(day) %>%
  dplyr::select(-location) %>% rename("horizon"=day)

latex_table <- xtable(table,
                      booktabs=T,
                      latex.environment ="center",
                      digits=2,
                      label=as.character(glue::glue("tab:nat_evo")),
                      caption=as.character(glue::glue("Model evaluations for predictions when all the models are fitted on the combined data from all the health zones."))
                      )

print.xtable(latex_table, type="latex", file="output/nat_tables.tex",include.rownames=FALSE)


table_hz <- overall_scores %>% filter(!(location %in% c("national", "national_combined")) & day %in% c(1,7,14,21,28)) %>%
  group_by(model, day) %>% summarize(crps=mean(crps), dss=mean(dss), calibration=mean(calibration),
                           centrality=mean(centrality), sharpness=mean(sharpness),
                           bias=mean(bias)) %>% arrange(day) %>% rename("horizon"=day)

latex_table_hz <- xtable(table_hz,
                      booktabs=T,
                      latex.environment ="center",
                      digits=2,
                      label=as.character(glue::glue("tab:hz_evo")),
                      caption=as.character(glue::glue("Model evaluations averaged over all the health-zones"))
                      )

print.xtable(latex_table_hz, type="latex", file="output/hz_tables.tex")


table_by_hz <- overall_scores %>% filter(!(location %in% c("national", "national_combined")) & model== "Poisson Semilocal" & day %in% c(7)) %>%
  group_by(location, day) %>% summarize(crps=mean(crps), dss=mean(dss), calibration=mean(calibration),
                           centrality=mean(centrality), sharpness=mean(sharpness),
                           bias=mean(bias)) %>% arrange(crps) %>% rename("horizon"=day)

latex_table_by_hz <- xtable(table_by_hz,
                      booktabs=T,
                      latex.environment ="center",
                      digits=2,
                      label=as.character(glue::glue("tab:by_hz_evo")),
                      caption=as.character(glue::glue("Model evaluations for the Poisson Semilocal model for each health zone"))
                      )

print.xtable(latex_table_by_hz, type="latex", file="output/by_hz_tables.tex")

tot_data <- rbind(tot_data, data.table(health_zone="national", cases=sum(tot_data[, cases])))

largest <- overall_scores %>%filter(calibration >=0.1) %>% group_by(location) %>%
  filter(day==max(day)) %>% dplyr::select(location, model,day) %>%
  group_by(location) %>% summarize(day=max(day), model=paste0(model, collapse=", "))%>%
  right_join(tot_data, by=c("location"="health_zone"))  %>%
  arrange(-day) %>% mutate(day=replace_na(day, "NA"), model=replace_na(model, "No calibrated model")) %>%
  filter(location != "N/A") %>%
  dplyr::select(Location=location, "Largest Horizon"=day,
                "Best model"=model, "Cases"= cases) 


latex_table_max_calibration <- xtable(largest,
                                      booktabs=T,
                                      latex.environment ="center",
                                      digits=0,
                                      label=as.character(glue::glue("tab:best_model")),
                                      caption="For each health zone we show the best maximal forecasting horizon where we can not exclude calibration at the p=0.1 level. We also show which models could provide this calibrated forecast")

align(latex_table_max_calibration) <- "l|l|l|p{8cm}|l|"

print.xtable(latex_table_max_calibration, type="latex", file="output/best_hz.tex",
             include.rownames=FALSE)


                                        #Health zones
model_conf_poisson <- list(
  desc="poisson_latest",
  new_cases=new_cases_poisson,
  R_func=R_latest_value)

model_conf_nbin <- list(
  desc="nbin_latest",
  new_cases=new_cases_neg_binom,
  R_func=R_latest_value)


for(hz in unique(overall_scores %>% pull(location))){

  if(hs == "national_combined"){
    next
  }
    
  plot_scores(overall_scores %>% filter(location == hz), hz)
  print(hz)
  if(hz %in% c("national")){
    data = model_data$national
  }else{
    data = model_data$health_zone[[hz]]
  }

  if( grepl(largest %>% filter(Location == hz)%>% pull("Best model") , "NegBin Semilocal", fixed=TRUE)){
    model_conf = model_conf_nbin
  }else{
    model_conf = model_conf_poisson
  }
  print(model_conf$desc)
  
  plot_preds(model_conf,data , hz,results)    

}


# BEST model




plot_preds(model_conf, model_data$health_zone$Beni, "Beni", results)
plot_preds(model_conf, model_data$health_zone$Katwa, "Katwa", results)



nat <- results %>% filter(location == "national" & day==1 &
                            model=="poisson_bsts")
predictions <- as.matrix(nat %>% dplyr::select(starts_with("V", ignore.case = FALSE)))
mean_I <- rowQuantiles(predictions, probs=0.5)

nat <- results %>% filter(location == "national_combined" & day==1 &
                            model=="poisson_bsts")
predictions <- as.matrix(nat %>% dplyr::select(starts_with("V", ignore.case = FALSE)))
mean_I_comb <- rowQuantiles(predictions, probs=0.5)
d = 1:length(mean_I)
length(mean_I)
q <- ggplot() + geom_line(aes(x=1:length(mean_I_comb), y=mean_I_comb)) # geom_line(aes(x=d, y=mean_I))#
ggsave("output/test.png", q)
