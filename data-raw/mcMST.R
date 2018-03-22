library(tidyverse)

mcMST = readr::read_delim("data-raw/mcMST.csv", delim = " ")

# 5 MB data should not be exceeded (https://cran.r-project.org/web/packages/policies.html)
# -> filter first three instances of size 40, 50 and 100 respectively
mcMST = mcMST[grepl("instance-(040|050|100)*-[1-3]*$", mcMST$prob.id, perl = TRUE), , drop = FALSE]

# rename stuff
mcMST$mutator = recode(mcMST$mutator,
  uniform = "ZHOU",
  edgeExchange = "EX",
  subgraphMST = "SG",
  mixed = "MIXED")

# collapse meta heuristic and mutation operator
mcMST$algorithm = paste(mcMST$emoatype, mcMST$mutator, sep = ".")
mcMST$algo.id = mcMST$emoatype = mcMST$mutator = NULL
print(head(mcMST))

colnames(mcMST) = c("f1", "f2", "prob", "repl", "algorithm")
devtools::use_data(mcMST, overwrite = T)
