library(Quandl)
library(dplyr)
library(zoo)
library(purrr)

symbols = as.list(read.delim('symbols.txt', sep = ',', header = FALSE))

x = 'CSCO'
symbol = x

leading = function(symbol){
  earnings = Quandl(paste('ZES/',symbol, sep = ''))
  stock = Quandl(paste('WIKI/',symbol,sep = ''), start_date=earnings$DATE[1]-40, end_date=earnings$DATE[1]-1)
  twentydaytrailing = stock[1:20,]
  twentydaytrailing = twentydaytrailing[,1:8]
  twentydaytrailing = twentydaytrailing %>% 
    mutate(pctChg = Close / Open -1) %>% 
    mutate(volChg = Volume / mean(twentydaytrailing$Volume))
  
  industrial = Quandl('YAHOO/INDEX_DJI', start_date=earnings$DATE[1]-30, end_date=earnings$DATE[1]-1)
  industrial = industrial %>% 
    mutate(indPctChg = Close / Open -1) %>% 
    mutate(indVolChg = Volume / mean(industrial$Volume))
  industrial = industrial[,c(1,8:9)]
  
  twentydaytrailing = merge(twentydaytrailing, industrial, all = TRUE)
  twentydaytrailing = twentydaytrailing %>% 
    mutate(Outperfom = pctChg>indPctChg) %>% 
    mutate(Volume = volChg-indVolChg) %>% 
    mutate(Change = pctChg-indPctChg) %>% 
    arrange(desc(Date))

 twentydaytrailing <<- na.omit(twentydaytrailing)
}

leading(x)

announce = function(symbol){
  earnings = Quandl(paste('ZES/',symbol, sep = ''))
  stock = Quandl(paste('WIKI/',symbol,sep = ''), start_date=earnings$DATE[1]-2, end_date=earnings$DATE[1]+3)
  index = Quandl('YAHOO/INDEX_DJI', start_date=earnings$DATE[1]-2, end_date=earnings$DATE[1]+3)
  astock = arrange(stock, desc(Date))
  aindex = arrange(index, desc(Date))
  perf <<- (astock[1,5]/astock[nrow(astock),2])-(aindex[1,5]/aindex[nrow(aindex),5])
}

announce(x)

# pull all earnings reporting days available 
count = 1
for(Symbol in symbols){
  temp = Quandl(gsub("'",'',paste('ZES/',Symbol, sep = '')))
  if(count == 1){
    total = data.frame(Symbol,temp)
    count = count + 1
  }else{
    hold = data.frame(Symbol, temp)
    total = rbind(total, hold)
    count = count + 1
  }
}
total$Symbol = gsub(pattern = "'",replacement = '',x = total$Symbol)
total = total[,c(2,1,3:4,6:7)]

# create a test data set to calculate performance
test = total[1:10,]

#loop through to calculate stock performace relative to market performance
for(i in 1:nrow(test)){
  t = Quandl(paste('WIKI/', test[i,2], sep = ''), start_date = test$DATE[i]-4, end_date=test$DATE[i]+5)
  stockperf = t[1,5]/t[nrow(t),2]
  market = Quandl('YAHOO/INDEX_DJI', start_date= test$DATE[i]-4, end_date=test$DATE[i]+5)
  marketperf = market[1,5]/market[nrow(market),2]
  relative = (stockperf/marketperf) - 1
  if(i == 1){
    test = mutate(test, StockPerf = stockperf - 1)
    test = mutate(test, MarketPerf = marketperf - 1)
    test = mutate(test, RelativePerf = relative)
  }else{
    test[i,7] = stockperf - 1
    test[i,8] = marketperf - 1
    test[i,9] = relative
  }
  print(i)
}

# add variable to show outperformers
test = mutate(test, Outperformer = (test$StockPerf>0 & test$RelativePerf>0))

# run through full earning dataset
for(i in 1:nrow(total)){
  t = Quandl(paste('WIKI/', total[i,2], sep = ''), start_date = total$DATE[i]-4, end_date=total$DATE[i]+5)
  stockperf = t[1,5]/t[nrow(t),2]
  market = Quandl('YAHOO/INDEX_DJI', start_date= total$DATE[i]-4, end_date=total$DATE[i]+5)
  marketperf = market[1,5]/market[nrow(market),2]
  relative = (stockperf/marketperf) - 1
  if(i == 1){
    total = mutate(total, StockPerf = stockperf - 1)
    total = mutate(total, MarketPerf = marketperf - 1)
    total = mutate(total, RelativePerf = relative)
  }else{
    total[i,7] = stockperf - 1
    total[i,8] = marketperf - 1
    total[i,9] = relative
  }
  print(i)
}

# add categorical for stock up and market outperform
total = mutate(total, Outperformer = (total$StockPerf>0 & total$RelativePerf>0))

# experimenting with how to get correct number of trading days in data pulls
weekdays(total[1,1], abbreviate = TRUE)
if(weekdays(total[1,1], abbreviate = TRUE))
  
  for(i in as.list(total[,1])){
    if(i %in% c('Wed','Thu','Fri')){
      print(i)
    }
  }

# experimenting with possible variables to include

# creating a count of high volume trading days
length(twentydaytrailing$Volume[twentydaytrailing$Volume>(mean(twentydaytrailing$Volume)+.5*sd(twentydaytrailing$Volume))])
length(twentydaytrailing$Change[twentydaytrailing$Change>(mean(twentydaytrailing$Change)+.5*sd(twentydaytrailing$Change))])
# creating a count of low volume trading days
length(twentydaytrailing$Volume[twentydaytrailing$Volume<(mean(twentydaytrailing$Volume)-.5*sd(twentydaytrailing$Volume))])
length(twentydaytrailing$Change[twentydaytrailing$Change<(mean(twentydaytrailing$Change)-.5*sd(twentydaytrailing$Change))])

rollmean(twentydaytrailing$Change, k = 5)[1]
rollmean(twentydaytrailing$Volume, k = 5)[1]


plot(stock$Date,stock$Volume)
par(new=TRUE)
plot(stock$Date,stock$Change,,type="l",col="blue",xaxt="n",yaxt="n",xlab="",ylab="")
par(new=TRUE)
plot(stock$Date,stock$pctChg,,type="l",col="red",xaxt="n",yaxt="n",xlab="",ylab="")