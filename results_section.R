source("plots.R")
source("util.R")
library(dplyr)
library(ggplot2)
library(xtable)

data <- read_data("2019-09-01")


hz <- data$health_zone

q <- plot_weekly_incidence(data)

ggsave("output/epi_curve.png", q, width = 9, height=6)

tot_data <- hz[, .(cases = max(confirmed_cases)), by=health_zone]


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
  
q <- create_map(tot_data, "cases", "Total Cases", width=7)

ggsave("output/tot_map.png", q, width=7, height=7)




results <- readRDS("results/latest_working.RDS")

scores <- readRDS("results/scores.RDS")

overall_scores <- ungroup(scores$overall) %>% mutate(model=recode(model,
                                                "poisson_bsts"="Poisson Semilocal",
                                                "nbin_bsts"="Negative Binomial Semilocal",
                                                "poisson_latest"="Poisson Latest",
                                                "nbin_latest"="Negative Binomial Latest"))

q <- plot_r(results[["Basic model"]]$national$model$R, min(hz[, report_date]))

ggsave("output/nat_rep.png", q, width = 9, height=6)


plot_scores(overall_scores %>% filter(location=="national"))


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


table_hz <- overall_scores %>% filter(location != "national" & day %in% c(1,7,14,21,28)) %>%
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


table_by_hz <- overall_scores %>% filter(location != "national" & model== "Poisson Semilocal" & day %in% c(7)) %>%
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

  


