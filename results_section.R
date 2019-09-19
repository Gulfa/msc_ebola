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

ggsave("output/epi_curve.png", q, width = 9, height=7)
setDT(hz)
tot_data <- hz[, .(cases = max(confirmed_cases)), by=health_zone]

q <- create_map(tot_data, "cases", "Total Cases")
ggsave("output/tot_map.png", q, width=7, height=6)


mu <- 10
x <- 0:40
df <- data.frame(
  x=x,
  Poisson=dpois(x, lambda=mu),
  "Binomial8"=dnbinom(x, mu=mu, size=8),
  "Binomial3"=dnbinom(x, mu=mu, size=3),
  "Binomial1"=dnbinom(x, mu=mu, size=1),
  "Binomial5"=dnbinom(x, mu=mu, size=0.5)
) %>% gather("Distribution", "value", -x) %>% mutate(
                                                "Distribution" = recode(Distribution,
                                                                        "Binomial8" = "NegBin(k=8)",
                                                                        "Binomial3" = "NegBin(k=3)",
                                                                        "Binomial1" = "NegBin(k=1)",
                                                                        "Binomial5" = "NegBin(k=0.5)",
                                                                        
                                                                        ))

q <- ggplot(df) + geom_line(aes(x=x, y=value, color=Distribution), size=1.2) + scale_color_brewer("Models", palette = "Dark2") + theme_bw() +xlab("New Cases") + ylab("Probability mass") +  theme(text = element_text(size=18))
ggsave("output/prob_dist.png", q, width=7, height=4)
  




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
  dplyr::select(-location) %>% rename("Horizon"=day, "Model"=model, "Sharpness"=sharpness,
                                      "Bias"=bias, "CRPS"=crps, "DSS"=dss, "Centrality"=centrality,
                                      "Calibration"=calibration)

latex_table <- xtable(table,
                      booktabs=T,
                      latex.environment ="center",
                      digits=2,
                      label=as.character(glue::glue("tab:nat_evo")),
                      caption=as.character(glue::glue("Model evaluations for predictions with different forecasting horizons for the national level."))
                      )


print.xtable(latex_table, type="latex", file="output/nat_tables.tex",include.rownames=FALSE,
             hline.after = c(0,4,8,12,16, 20))


## table_hz <- overall_scores %>% filter(!(location %in% c("national", "national_combined")) & day %in% c(1,7,14,21,28)) %>%
##   group_by(model, day) %>% summarize(crps=mean(crps), dss=mean(dss), calibration=mean(calibration),
##                            centrality=mean(centrality), sharpness=mean(sharpness),
##                            bias=mean(bias)) %>% arrange(day) %>% rename("horizon"=day)

## latex_table_hz <- xtable(table_hz,
##                       booktabs=T,
##                       latex.environment ="center",
##                       digits=2,
##                       label=as.character(glue::glue("tab:hz_evo")),
##                       caption=as.character(glue::glue("Model evaluations averaged over all the health-zones"))
##                       )

## print.xtable(latex_table_hz, type="latex", file="output/hz_tables.tex")


## table_by_hz <- overall_scores %>% filter(!(location %in% c("national", "national_combined")) & model== "Poisson Semilocal" & day %in% c(7)) %>%
##   group_by(location, day) %>% summarize(crps=mean(crps), dss=mean(dss), calibration=mean(calibration),
##                            centrality=mean(centrality), sharpness=mean(sharpness),
##                            bias=mean(bias)) %>% arrange(crps) %>% rename("horizon"=day)

## latex_table_by_hz <- xtable(table_by_hz,
##                       booktabs=T,
##                       latex.environment ="center",
##                       digits=2,
##                       label=as.character(glue::glue("tab:by_hz_evo")),
##                       caption=as.character(glue::glue("Model evaluations for the Poisson Semilocal model for each health zone"))
##                       )

## print.xtable(latex_table_by_hz, type="latex", file="output/by_hz_tables.tex")

tot_data <- rbind(tot_data, data.table(health_zone="national", cases=sum(tot_data[, cases])))
f <- function(x){
  if(length(x) > 1){
    return("All models")
  }else{
    return(x)
  }
}

largest <- overall_scores %>%filter(calibration >=0.1) %>% group_by(location) %>%
  filter(day==max(day))  %>% dplyr::select(location, model,day, crps) %>%
  group_by(location) %>% filter(crps==min(crps)) %>% summarize(day=max(day), model=f(model))%>%
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
                                      caption="For each health zone we show the maximal forecasting horizon where we can not exclude calibration at the p=0.1 level. If multiple models are equally calibrated we chosse the one with smallest CRPS. For some health zones there were no calibrated forecasts")

align(latex_table_max_calibration) <- "l|l|l|l|l|"

print.xtable(latex_table_max_calibration, type="latex", file="output/best_hz.tex",
             include.rownames=FALSE, table.placement="h!")


                                        #Health zones
model_conf_poisson <- list(
  desc="poisson_latest",
  new_cases=new_cases_poisson,
  R_func=R_latest_value)

model_conf_nbin <- list(
  desc="nbin_latest",
  new_cases=new_cases_neg_binom,
  R_func=R_latest_value)

model <- fit_model(model_data$national, model_conf$desc, model_conf$new_cases, model_conf$R_func)

plot_r(model$R, model_data$national$dates[1])



output=""
for(hz in unique(overall_scores %>% pull(location))){

  if(hz == "national_combined"){
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
  
  output = paste(output,
                 glue::glue(
'\\section{{ {hz} }}',
'\\begin{{figure}}[H]',
'\\begin{{subfigure}}{{\\textwidth}}',
'  \\centering',
'  \\includegraphics[width=0.9\\linewidth, height=7cm]{{../output/{hz}_predictions.png}}',
'  \\caption{{Forecasted and predicted incidence for the best fitting model}}',
'\\end{{subfigure}}\n\n',
'\\begin{{subfigure}}{{\\textwidth}}',
'  \\centering',
'  \\includegraphics[width=0.9\\linewidth, height=7cm]{{../output/{hz}_Rs.png}}',
'  \\caption{{Forecasted and predicted repreoduction numbers for the semilocal poisson model}}',
'\\end{{subfigure}}',
'  \\caption{{Median forecast with 95 \\% prediction intervals and observed values for incidence and reproduction number for the semilocal poisson model for {hz}.}}',
'\\end{{figure}}\n\n',
"\\begin{{figure}}[H]
\\begin{{subfigure}}{{0.5\\textwidth}}
  \\centering
  \\includegraphics[width=\\linewidth]{{../output/{hz}_crps.png}}  
  \\caption{{Contineously Ranked Probability Score}}
  \\label{{{hz}_scores_1}}
\\end{{subfigure}}
\\begin{{subfigure}}{{0.5\\textwidth}}
  \\centering
  \\includegraphics[width=\\linewidth]{{../output/{hz}_calibration.png}}  
  \\caption{{Calibration p-value}}
  \\label{{{hz}_scores_2}}
\\end{{subfigure}}

\\begin{{subfigure}}{{0.5\\textwidth}}
  \\centering
  \\includegraphics[width=\\linewidth]{{../output/{hz}_bias.png}}  
  \\caption{{Bias}}
  \\label{{fig:{hz}_scores_3}}
\\end{{subfigure}}
\\begin{{subfigure}}{{0.5\\textwidth}}
  \\centering
  \\includegraphics[width=\\linewidth]{{../output/{hz}_centrality.png}}  
  \\caption{{Centrality of PIT values}}
  \\label{{fig:{hz}_scores_4}}
\\end{{subfigure}}
  \\caption{{Scores for {hz} as a function of the forecasting horizon.}}

  \\label{{fig:nat_scores}}
\\end{{figure}}

"))
}

file = "output/appendix_plots.tex"
cat(output, file=file)
# BEST model





nat <- results %>% filter(location == "national" & day==1 &
                            model=="poisson_bsts")
predictions <- as.matrix(nat %>% dplyr::select(starts_with("V", ignore.case = FALSE)))
mean_I <- rowQuantiles(predictions, probs=0.5)
quantiles <- rowQuantiles(predictions, probs=c(0.05, 0.95))
d = 1:length(mean_I)
plot_data <- data.frame(median = mean_I, low=quantiles[,1], high=quantiles[,2],
                  Model="National model", d=d)


nat <- results %>% filter(location == "national_combined" & day==1 &
                            model=="poisson_bsts")
predictions <- as.matrix(nat %>% dplyr::select(starts_with("V", ignore.case = FALSE)))
mean_I_comb <- rowQuantiles(predictions, probs=0.5)
quantiles_comb <- rowQuantiles(predictions, probs=c(0.05, 0.95))
plot_data <- rbind(plot_data,
                   data.frame(d=d,
                              median = mean_I_comb, low=quantiles_comb[,1], high=quantiles_comb[,2],
                              Model="Combined Health Zones"))




length(mean_I)
q <- ggplot(plot_data) + geom_line(aes(x=d, y=median, color=Model)) +
  geom_ribbon(aes(x=d, ymin=low, ymax=high, fill=Model),alpha=0.4) + 
  theme_bw() + xlab("Days") + ylab("Incidence") + scale_color_brewer("Models", palette = "Dark2") +
  scale_fill_brewer("Models", palette = "Dark2")
#  scale_fill_manual("Model", labels=c("red"="Sum of Health Zones", "blue"="National model"))
ggsave("output/nat_combined.png", q, width=8, height=4)

