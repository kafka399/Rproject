#Author: Dzidorius Martinaitis
#Date: 2011-01-11
#Description: http://www.investuotojas.eu/?p=447

require('xts')
require('quantmod')
Sys.setenv(TZ="GMT")

require('PerformanceAnalytics')

tmp<-as.matrix(read.table('tickers/various_day_close/rb_contract1.csv',sep=',',header=FALSE))
rb<-as.xts(as.double(tmp[,2]),order.by=as.POSIXct(strptime(tmp[,1],'%Y-%m-%d')))

rb<-tail(rb,-3)['::2010-11']


tmp<-as.matrix(read.table('tickers/various_day_close/cl_contract1.csv',sep=',',header=FALSE))
cl<-as.xts(as.double(tmp[,2]),order.by=as.POSIXct(strptime(tmp[,1],'%Y-%m-%d')))
cl<-tail(cl,-3)['::2010-11']


rb.delta<-Delt(((rb)))['1997-01::']
cl.delta<-Delt(((cl)))['1997-01::']

rb.delta[is.na(rb.delta)]<-0
cl.delta[is.na(cl.delta)]<-0

spread<-cl.delta*50000-rb.delta*50000

png('spread_cl_prices.png',width=650)
chart.CumReturns(cbind(cl.delta,rb.delta),col=c(2,3),main='Oil & Gasoline prices')
dev.off()

png('spread_cl_rb.png',width=650)
chart.TimeSeries(cumsum(spread),main='Seasonal spread: CL vs RB')
dev.off()

spread<-cl.delta-rb.delta

png('spread_cl_rb_prc.png',width=650)
chart.CumReturns((spread),main='Seasonal spread %: CL vs RB')
dev.off()

spread.factor<-as.factor(format(index(spread),'%m'))
aggregate(spread, spread.factor,mean)
summary(lm(as.double(spread)~(spread.factor)))



rb.delta.monthly<-Delt(Cl(to.monthly(rb,indexAt='endof')))['1997-01::']
cl.delta.monthly<-Delt(Cl(to.monthly(cl,indexAt='endof')))['1997-01::']

rb.delta.monthly[is.na(rb.delta.monthly)]<-0
cl.delta.monthly[is.na(cl.delta.monthly)]<-0

factor<-as.factor(format(index(cl.delta.monthly-rb.delta.monthly),'%m'))
tmp<-data.frame(as.double(cl.delta.monthly-rb.delta.monthly),as.numeric(factor))

require('ggplot2')
png('monthly_averages.png',width=650)
qplot(factor(as.numeric(factor)),as.double(cl.delta.monthly-rb.delta.monthly),data=tmp,geom = "boxplot",ylab='Monthly average returns',xlab='Months')
dev.off()

png('march_cumulative.png',width=650)
chart.CumReturns(spread[spread.factor=='03'],main='March cumulative return')
dev.off()

png('yearly_diff.png',width=650)
chart.TimeSeries(cbind(as.xts(rollapply(rb.delta,250,sd,align='right'))-as.xts(rollapply(cl.delta,250,sd,align='right'))),main='Yearly difference of vol. between CL & RB')
dev.off()