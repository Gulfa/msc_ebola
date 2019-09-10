library(data.table)
source("evaluate.R")

output <- readRDS("results/latest.RDS")
scores <- evaluate(output, cores=10)

saveRDS(scores, "results/scores.RDS")
