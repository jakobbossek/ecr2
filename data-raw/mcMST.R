library(tidyverse)

mcMST = readr::read_delim("data-raw/mcMST.csv", delim = " ")

# remove the following lines
#mcMST = mcMST[grepl("instance-100", mcMST$prob.id), , drop = FALSE]
#mcMST = subset(mcMST, emoatype == "NSGA2")
mcMST$mutator = recode(mcMST$mutator, uniform = "ZHOU",
  edgeExchange = "EX", subgraphMST = "SG", mixed = "MIXED")
mcMST$algorithm = paste(mcMST$emoatype, mcMST$mutator, sep = ".")
mcMST$algo.id = mcMST$emoatype = mcMST$mutator = NULL
print(head(mcMST))

colnames(mcMST) = c("f1", "f2", "prob", "repl", "algorithm")
devtools::use_data(mcMST, overwrite = T)
