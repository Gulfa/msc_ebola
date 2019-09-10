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
                                                "nbin_bsts"="Negative Binomial Semilocal",
                                                "poisson_latest"="Poisson Latest",
                                                "nbin_latest"="Negative Binomial Latest"))

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

print.xtable(latex_table, type="latex", file="output/nat_tables.tex")


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
                           bias=mean(bias)) %>% arrange(crps) %>% rename("horizon"=day) %>% inner_join(tot_data, by=c("location"="health_zone"))

latex_table_by_hz <- xtable(table_by_hz,
                      booktabs=T,
                      latex.environment ="center",
                      digits=2,
                      label=as.character(glue::glue("tab:by_hz_evo")),
                      caption=as.character(glue::glue("Model evaluations for the Poisson Semilocal model for each health zone"))
                      )

print.xtable(latex_table_by_hz, type="latex", file="output/by_hz_tables.tex")









# BEST model

model_conf <- list(
  desc="poisson_bsts",
  new_cases=new_cases_poisson,
  R_func=R_semilocal)


model <- fit_model(model_data$national, model_conf$desc, model_conf$new_cases, model_conf$R_func)

res <- results %>% filter(location == "national" & model=="poisson_bsts")

max_day = max(model_data$national$days)
show_days <- c(50,100, 150, 200, 250, 300, 350, max_day)

pred <- data.table(days=1:(max_day + 28),
                   pred=as.numeric(NA),
                   upper=as.numeric(NA),
                   lower=as.numeric(NA),
                   value=c(model_data$national$incidence, NA*numeric(length=28)))
Rs <-  data.table(days=min(model$R[["t_end"]]):(max_day + 28),
                   pred=as.numeric(NA),
                   upper=as.numeric(NA),
                   lower=as.numeric(NA),
                   value=c(model$R[["Mean(R)"]], NA*numeric(length=28)))



for(show_day in show_days){
  print(show_day)
  predictions <- res %>%  filter(start_day==show_day)
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

q <- ggplot(pred) + geom_line(aes(x=days, y=value)) +
  geom_line(aes(x=days, y=pred), color="red") +
  geom_ribbon(aes(x=days, ymin=lower, ymax=upper, group=1), alpha=0.4) 

ggsave("output/national_predictions.png", q)


q <- ggplot(Rs) + geom_line(aes(x=days, y=value)) +
  geom_line(aes(x=days, y=pred), color="red") +
  geom_ribbon(aes(x=days, ymin=lower, ymax=upper, group=1), alpha=0.4) 
ggsave("output/national_Rs.png", q)



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
