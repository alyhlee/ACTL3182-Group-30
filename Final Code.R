library(BatchGetSymbols)
library(tidyverse)
library(tidyquant)
library(PerformanceAnalytics)
library(quantmod)
library(IntroCompFinR)
library(stocks)
library(NMOF)


stock_returns<-read.csv('stock_returns.csv', header = TRUE, row.names = 1)
stock_names <- colnames(stock_returns)

#setup for financial data clustering
df.SP500 <- GetSP500Stocks()
tickers <- stock_names

what_metrics <- yahooQF(c( "Symbol",
                           "Price/Book",
                           "Earnings/Share",
                           "Last Trade (Price Only)",
                           "Market Capitalization",
                           "P/E Ratio"
))

metrics <- getQuote(paste(tickers, sep="", collapse=";"), what=what_metrics, )
colnames(metrics) = c("time", "Ticker", "pricebook","EPS",
                      "last.price", "Marketcap", "PE")



metricspricebook<-metrics[, c(2,3)] %>% arrange(pricebook)
metricsEPS<-metrics[, c(2, 4)] %>% arrange(EPS)
metricslast.price<-metrics[, c(2,5)] %>% arrange(last.price)
metricsMarketcap<-metrics[, c(2,6)] %>% arrange(Marketcap)
metricsPE<-metrics[, c(2,7)] %>% arrange(PE)

#Rerun this section replacing pricebook with EPS, last.price, Markecap and PE
#careful line 92, will have to change depending on metric

Marketcapdata<-as.data.frame(metricsMarketcap$Ticker[which(metricsMarketcap$Marketcap>=0)])
nrow(Marketcapdata)
Marketcapdata.number<-round(nrow(Marketcapdata)/8)

Marketcap1 <- Marketcapdata[1:Marketcapdata.number,]
Marketcap2 <- Marketcapdata[(Marketcapdata.number+1):(2*Marketcapdata.number),]
Marketcap3 <- Marketcapdata[(2*Marketcapdata.number+1):(3*Marketcapdata.number),]
Marketcap4 <- Marketcapdata[(3*Marketcapdata.number+1):(4*Marketcapdata.number),]
Marketcap5 <- Marketcapdata[(4*Marketcapdata.number+1):(5*Marketcapdata.number),]
Marketcap6 <- Marketcapdata[(5*Marketcapdata.number+1):(6*Marketcapdata.number),]
Marketcap7 <- Marketcapdata[(6*Marketcapdata.number+1):(7*Marketcapdata.number),]
Marketcap8 <- Marketcapdata[(7*Marketcapdata.number+1):(8*Marketcapdata.number-2),]

clusterMarketcap1 <- Return.calculate(rowSums((indexedprice[,c(Marketcap1)]/length(Marketcap1))))[-1,]
clusterMarketcap2 <- Return.calculate(rowSums((indexedprice[,c(Marketcap2)]/length(Marketcap2))))[-1,]
clusterMarketcap3 <- Return.calculate(rowSums((indexedprice[,c(Marketcap3)]/length(Marketcap3))))[-1,]
clusterMarketcap4 <- Return.calculate(rowSums((indexedprice[,c(Marketcap4)]/length(Marketcap4))))[-1,]
clusterMarketcap5 <- Return.calculate(rowSums((indexedprice[,c(Marketcap5)]/length(Marketcap5))))[-1,]
clusterMarketcap6 <- Return.calculate(rowSums((indexedprice[,c(Marketcap6)]/length(Marketcap6))))[-1,]
clusterMarketcap7 <- Return.calculate(rowSums((indexedprice[,c(Marketcap7)]/length(Marketcap7))))[-1,]
clusterMarketcap8 <- Return.calculate(rowSums((indexedprice[,c(Marketcap8)]/length(Marketcap8))))[-1,]

Marketcapreturnsdf <- cbind(clusterMarketcap1, clusterMarketcap2, clusterMarketcap3, clusterMarketcap4, clusterMarketcap5,
                            clusterMarketcap6, clusterMarketcap7, clusterMarketcap8)

#finding the tangency portfolio and its weights

Returns<-Marketcapreturnsdf[1:3020,]

rf<- 1.0417^(1/252)-1
cov.mat<-cov(Returns,use="complete.obs")
mu.vec<-colMeans(Returns,na.rm=TRUE)

gmin.port <- globalMin.portfolio(mu.vec, cov.mat,shorts=F)

tan.port <- tangency.portfolio(mu.vec, cov.mat, rf,shorts=F)


ef <-efficient.frontier(mu.vec, cov.mat, alpha.min=-10, 
                        alpha.max=10, nport=50,shorts=F)

plot(ef, plot.assets=T, col="blue", pch=16) 
points(gmin.port$sd, gmin.port$er, col="green", pch=16, cex=2) 
text(gmin.port$sd, gmin.port$er, labels = "Global min", pos = 4) 
points(tan.port$sd, tan.port$er, col="red", pch=16, cex=2) 
text(tan.port$sd, tan.port$er, labels = "Tangency", pos = 3) 
sr.tan = (tan.port$er - rf)/tan.port$sd 
abline(a=rf, b=sr.tan, col="green", lwd=2)


#set up for finding metrics 

Returns <- unname(Marketcapreturnsdf[3021:3980,])

TangencyReturns<-as.matrix(Returns)%*%tan.port$weights 
dates<-row.names(stock_returns[3021:3980,])

TangencyReturns_dataframe<-data.frame(TangencyReturns,row.names = as.Date(dates))

TimeSeries_TangencyReturns<-xts(TangencyReturns_dataframe$TangencyReturns, order.by = as.Date(dates))

#portfolio measures on tangency portfolio
sharpe<-SharpeRatio(TimeSeries_TangencyReturns,Rf=rf)
annualsharpe<-SharpeRatio.annualized(TimeSeries_TangencyReturns,Rf=rf,scale=252, geometric=FALSE)
adjsharpe<-AdjustedSharpeRatio(TimeSeries_TangencyReturns,Rf=rf, scale = 252, geometric = FALSE)
sortino<-sortino(TangencyReturns,rf=rf)

summary(TangencyReturns)
skewness(TangencyReturns)
sd(TangencyReturns)
kurtosis(TangencyReturns)

divrat<-divRatio(tan.port$weights,cov.mat)
downsidedev<-DownsideDeviation(TimeSeries_TangencyReturns,MAR = 0) #sMarketcapcify MAR
SemiDeviation(TimeSeries_TangencyReturns)
SemiVariance(TimeSeries_TangencyReturns)

sspw<-sum(tan.port$weights^2)
