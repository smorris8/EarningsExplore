library(Quandl)
library(dplyr)
library(zoo)
library(purrr)

symbols = as.list(read.delim('symbols.txt', sep = ',', header = FALSE))

x = 'CSCO'

leading = function(symbol){
  earnings = Quandl(paste('ZES/',symbol, sep = ''))
  stock = Quandl(paste('WIKI/',symbol,sep = ''), start_date=earnings$DATE[1]-30, end_date=earnings$DATE[1]-1)
  stock = stock[,1:8]
  stock = stock %>% 
    mutate(pctChg = Close / Open -1) %>% 
    mutate(volChg = Volume / mean(stock$Volume))
  
  industrial = Quandl('YAHOO/INDEX_DJI', start_date=earnings$DATE[1]-30, end_date=earnings$DATE[1]-1)
  industrial = industrial %>% 
    mutate(indPctChg = Close / Open -1) %>% 
    mutate(indVolChg = Volume / mean(industrial$Volume))
  industrial = industrial[,c(1,8:9)]
  
  stock = merge(stock, industrial, all = TRUE)
  stock = stock %>% 
    mutate(Outperfom = pctChg>indPctChg) %>% 
    mutate(Volume = volChg/indVolChg) %>% 
    mutate(Change = pctChg/indPctChg) %>% 
    arrange(desc(Date))
  
 stock <<- stock
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


plot(stock$Date,stock$Volume)
par(new=TRUE)
plot(stock$Date,stock$Change,,type="l",col="blue",xaxt="n",yaxt="n",xlab="",ylab="")
par(new=TRUE)
plot(stock$Date,stock$pctChg,,type="l",col="red",xaxt="n",yaxt="n",xlab="",ylab="")
