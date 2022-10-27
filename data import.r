library('quantmod')
library('moments')
library('PerformanceAnalytics')

stock_names<-read.csv('list-of-s-and-p-500-companies.csv', header = TRUE)
stock_codes<-stock_names$Symbol
dataset_size<-nrow(getSymbols("AAPL", src = 'yahoo', auto.assign = FALSE))
getReturns<-function(symbol, method, size){
  temp_xts<-getSymbols(symbol,src = 'yahoo', auto.assign = FALSE)
  return<-as.matrix(CalculateReturns(temp_xts[,6], method = method))
  if(nrow(return) != size) {
    return<-matrix(0, size)
  } else {
    return
  }
}
'use method = "log" for log returns instead'
return_dataset<-sapply(stock_codes,getReturns, method = "discrete", size = dataset_size)
'remove NA returns'
return_dataset<-return_dataset[-1,]
return_dataset<-return_dataset[,colSums(return_dataset) != 0]
'standardised time period of data'
return_dataset<-return_dataset[1:3980,]
write.csv(return_dataset, "stock_returns.csv")
log_returns<-log(1+return_dataset)
write.csv(log_returns, "log_returns.csv")
