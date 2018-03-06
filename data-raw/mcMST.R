library(tidyverse)

mcMST = read.table("data-raw/mcMST.csv", header = TRUE, stringsAsFactors = FALSE)

# remove the following lines
mcMST = mcMST[grepl("instance-100", mcMST$prob.id), , drop = FALSE]
mcMST = subset(mcMST, emoatype == "NSGA2")
mcMST$mutator = recode(mcMST$mutator, uniform = "EMOA.ZHOU",
  edgeExchange = "EMOA.EX", subgraphMST = "EMOA.SG", mixed = "EMOA.MIXED")
mcMST$algo.id = mcMST$emoatype = NULL
print(head(mcMST))

colnames(mcMST) = c("f1", "f2", "prob", "repl", "algorithm")
devtools::use_data(mcMST, overwrite = T)
