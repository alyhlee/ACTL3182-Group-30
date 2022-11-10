library(tidyverse)
library(HierPortfolios)
return_dataset<-read.csv("stock_returns.csv", header = TRUE, row.names = 1)
training_dataset<-return_dataset[1:3020,]
testing_dataset<-return_dataset[3021:3980,]
covar<-cov(training_dataset)
HC_Portfolio<-HCAA_Portfolio(covar, graph = TRUE)


HierarchicalReturns<-as.matrix(testing_dataset)%*%HC_Portfolio$weights 


HierarchicalReturns_dataframe<-data.frame(HierarchicalReturns,row.names = row.names(testing_dataset))

TimeSeries_HierarchicalReturns<-xts(HierarchicalReturns_dataframe$HierarchicalReturns, order.by = as.Date(row.names(HierarchicalReturns_dataframe)))
HierarchicalReturns<-as.vector(HierarchicalReturns)
save(HierarchicalReturns,file="HierarchicalReturns.RData")

#portfolio measures on portfolio
library(NMOF)
library(stocks)
library(PerformanceAnalytics)
rf<-(1.0417)^(1/252) - 1
sharpe<-SharpeRatio(TimeSeries_HierarchicalReturns,Rf=rf)
annualsharpe<-SharpeRatio.annualized(TimeSeries_HierarchicalReturns,Rf=rf,scale=252, geometric = FALSE)
adjsharpe<-AdjustedSharpeRatio(TimeSeries_HierarchicalReturns,Rf=rf, scale = 252, geometric = FALSE)
sortino<-sortino(HierarchicalReturns,rf=rf)


summary(HierarchicalReturns)
skewness(HierarchicalReturns)
sd(HierarchicalReturns)
kurtosis(HierarchicalReturns)
hist(HierarchicalReturns, breaks=30)

divrat<-divRatio(HC_Portfolio$weights,covar)
downsidedev<-DownsideDeviation(TimeSeries_HierarchicalReturns,MAR = 0)
SemiSD(TimeSeries_HierarchicalReturns)
SemiVariance(TimeSeries_HierarchicalReturns)


sspw<-sum(HC_Portfolio$weights^2)
