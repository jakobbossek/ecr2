library(tidyverse)

mcMST = read.table("data-raw/mcMST.csv", header = TRUE, stringsAsFactors = FALSE)
colnames(mcMST) = c("f1", "f2", "algorithm", "prob")
devtools::use_data(mcMST, overwrite = T)
