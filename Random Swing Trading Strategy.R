## GET SYMBOLS
install.packages()
library(XML)
library(RCurl)
library(rlist)
theurl <- getURL("https://en.wikipedia.org/wiki/List_of_S%26P_500_companies",.opts = list(ssl.verifypeer = FALSE) )
tables <- readHTMLTable(theurl)
tables <- list.clean(tables, fun = is.null, recursive = FALSE)
n.rows <- unlist(lapply(tables, function(t) dim(t)[1]))
ticker<-tables[[which.max(n.rows)]]$V1
symbols<-as.character(ticker)
symbols<-symbols[-1]

#Get data for multiple tickers, return a list
getMultipleData<-function(ticker,start_date,end_date){
Data3<-new.env()
getSymbols(ticker,from=start_date,to=end_date,env=Data3)
Stocks<-eapply(Data3,merge)
return(Stocks)
}
library(quantmod)
newStocks<-getMultipleData(symbols,start_date="2019-12-25",end_date="2019-12-29")
head(newStocks)
library(data.table)
dailyReturn<-unlist(lapply(newStocks,function(m){as.data.table(dailyReturn(m))$daily.returns[2]}))
names<-names(head(sort(dailyReturn),20))
## The return in ten days of every ticker after experiencing a maximum draw down
PriceDrop<-function(stock){
  downPercentage<-min(dailyReturn(stock))
  index<-which(dailyReturn(stock)==min(dailyReturn(stock)))
  data<-Cl(stock[index,])
  data$downPercentage<-downPercentage
  return(data)
}

dailyReturn(FNKO)
getSymbols("SNAP",from=as.Date("2019-9-13"),to=as.Date("2019-10-18"))
PriceDrop(SNAP)
## Things to consider: narrow the time gap? Always pick the first several that has maximum draw down? during a certain time interval? 

Lookback<-function(stock,days){
start_date=as.Date("2015-3-25")
end_date=as.Date("2019-9-30")
watchlist<-getSymbols(stock,from=start_date,to=end_date,auto.assign = FALSE)
part<-seq(from=1,to=nrow(watchlist),by=20)
differentDate<-data.table()
for(i in 1:(length(part)-2)){
data<-watchlist[(part[i]:(part[i+1]-1)),]
index<-which(dailyReturn(data)==min(dailyReturn(data)))[1]
downTurn<-min(dailyReturn(data))
subset<-watchlist[(index+(i-1)*20):(index+(i-1)*20+days),]
EndPriceLargeDecrease<-Cl(subset)[1,]
EndPrice<-as.numeric(Hi(subset)[days+1,])
EndPercentage<-as.numeric((EndPrice-EndPriceLargeDecrease)/EndPrice)
MaxPrice<-as.numeric(max(Hi(subset[c(-1,-2),])))
daysMax<-which(Hi(subset)==MaxPrice)[1]
MaxPercentage<-as.numeric((MaxPrice-EndPriceLargeDecrease)/MaxPrice)
PriceHigherNextday<- as.numeric(Cl(subset)[2,])>as.numeric(Cl(subset)[1,])

subTable<-Cl(subset)[1,]
subTable$downTurn<-downTurn
subTable$EndPercentage<-EndPercentage
subTable$MaxPercentage<-MaxPercentage
subTable$daysMax<-daysMax
subTable$PriceHigherNextday<-TRUE
subTable$PriceHigherNextday<-PriceHigherNextday
subTable<-as.data.table(subTable)
subTable$PriceHigherNext<-subTable$PriceHigherNextday==1
subTable$PriceHigherNextday=NULL
differentDate<-rbind(differentDate,subTable)
}
#data.frame(names=stock,mean=mean(differentDate$EndPercentage),meanMax=mean(differentDate$MaxPercentage),sd=sd(differentDate$MaxPercentage))
list(data=differentDate,mean=mean(differentDate$EndPercentage),meanMax=mean(differentDate$MaxPercentage),sd=sd(differentDate$MaxPercentage))
}





Lookback("",15)
