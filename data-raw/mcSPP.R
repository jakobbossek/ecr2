library(tidyverse)

mcSPP = readr::read_delim("data-raw/mcSPP.csv", delim = " ")

# remove the following lines
#mcSPP = mcSPP[grepl("instance-100", mcSPP$prob.id), , drop = FALSE]
#mcSPP = subset(mcSPP, emoatype == "NSGA2")
# mcSPP$mutator = recode(mcSPP$mutator, uniform = "EMOA.ZHOU",
#   edgeExchange = "EMOA.EX", subgraphMST = "EMOA.SG", mixed = "EMOA.MIXED")
print(head(mcSPP))
idx.nonwsum = which(as.character(mcSPP$algorithm) != "WSUM")
print(idx.nonwsum)
mcSPP$algorithm[idx.nonwsum] = paste(mcSPP$emoa[idx.nonwsum], mcSPP$mutator[idx.nonwsum], sep = ".")
mcSPP$emoa = mcSPP$mutator = NULL
#mcSPP$algorithm = gsub("1.NA", "WSUM", mcSPP$algorithm)
#mcSPP$algorithm = recode(mcSPP$algorithm, "1.NA" = "WSUM")
#mcSPP = arrange(mcSPP, prob, algorithm, repl)
print(head(mcSPP))

#colnames(mcSPP) = c("f1", "f2", "prob", "repl", "algorithm")
devtools::use_data(mcSPP, overwrite = T)
