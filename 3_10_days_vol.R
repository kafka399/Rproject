require('xts')
require('quantmod')
Sys.setenv(TZ="GMT")

getSymbols(c('SPY','^VIX'),from='1995-01-01')

spy.delta<-Delt(Cl(SPY))

short.vol<-as.xts(rollapply(spy.delta,3,sd,align='right'))
long.vol<-as.xts(rollapply(spy.delta,10,sd,align='right'))

future.vol<-(head(lag(short.vol,-3),-3))

past.vol<-short.vol/long.vol

signal<-index(past.vol[past.vol<0.25])#ifelse(past.vol<0.3,1,0)



temp<-cbind(future.vol,short.vol)
temp<-(tail(temp,-1))
temp<-(head(temp,-3))

print('all days:')
summary(as.double(temp[,1])/as.double(temp[,2]))

print('days, then past volatility < 0.25:')
summary(as.double(temp[,1][signal])/as.double(temp[,2][signal]))

