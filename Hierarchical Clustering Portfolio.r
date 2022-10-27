library(tidyverse)
library(HierPortfolios)
return_dataset<-read.csv("stock_returns.csv", header = TRUE)[,-1]
covar<-cov(return_dataset)
HCAA_Portfolio(covar, graph = TRUE)
