source("evaluate.R")

output <- readRDS("results/latest.RDS")
scores <- evaluate(output, cores=4)

saveRDS(scores, "results/scores.RDS")
