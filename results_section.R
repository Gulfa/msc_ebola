source("plots.R")
library(dplyr)
library(ggplot2)
library(xtable)

data <- read_data("2019-07-04")


hz <- data$health_zone

q <- plot_weekly_incidence(data)

ggsave("output/epi_curve.png", q, width = 9, height=6)

tot_data <- hz[, .(cases = max(total_cases)), by=health_zone]

  
q <- create_map(tot_data, "cases", "Total Cases")

ggsave("output/tot_map.png", q, width=7, height=




results <- readRDS("results/latest_working.RDS")

q <- plot_r(results[["Basic model"]]$national$model$R, min(hz[, report_date]))
ggsave("output/nat_rep.png", q, width = 9, height=6)



table <- create_score_table_national(results)
latex_table <- xtable(table,
                      booktabs=T,
                      latex.environment ="center",
                      digits=2,
                      label=as.character(glue::glue("tab:nat_evo")),
                      caption=as.character(glue::glue("Model evaluations for predictions when all the models are fitted on the combined data from all the health zones."))
                      )

print.xtable(latex_table, type="latex", file="output/nat_tables.tex")


table <- create_score_table_regional(results)
latex_table <- xtable(table,
                      booktabs=T,
                      latex.environment ="center",
                      digits=2,
                      label=as.character(glue::glue("tab:hz_evo")),
                      caption=as.character(glue::glue("Model evaluations averaged over all health zones "))
                      )

print.xtable(latex_table, type="latex", file="output/hz_tables.tex")
  

create_score_table_by_zone(results, "Basic model")
