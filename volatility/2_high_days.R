#If during the last two days VIX return was above 6%, then there is high probability, that return of S&P500 index will be positive on third day 
#author Dzidorius Martinaitis
#2010-12-29
# http://www.investuotojas.eu/?p=439
require('xts')
require('quantmod')
require('blotter')
require('PerformanceAnalytics')
require('FinancialInstrument')
Sys.setenv(TZ="GMT")

#data part
getSymbols(c('SPY','^VIX'),from='1995-01-01',index.class=c("POSIXt","POSIXct"))
dividends<-getDividends('SPY',from='1995-01-01',index.class=c("POSIXt","POSIXct"))

temp<-cbind(dividends,SPY)
temp[,1][is.na(temp[,1])]<-0

SPY<-cbind(temp[,2],temp[,3],temp[,4],temp[,1]+temp[,5])
colnames(SPY)<-c('Open','High','Low','Close')
spy.delta<-Delt(Cl(SPY))

vix.delta<-Delt(Cl(VIX))

signal<-ifelse(vix.delta>0.06& lag(vix.delta>0.06,1),1,0)
png('2highdays.png',width=650)
chart.CumReturns(lag(signal,1)*spy.delta,main='when VIX >6% during two days')
dev.off()

#blotter code
symbols<-c('SPY')
SPY<-Cl(SPY)
initDate=time(get(symbols)[1])
initEq=50000
rm(list=ls(envir=.blotter),envir=.blotter)
ltportfolio='2high'
ltaccount='2high'
initPortf(ltportfolio,symbols, initDate=initDate)
initAcct(ltaccount,portfolios=c(ltportfolio), initDate=initDate,initEq=initEq)
currency("USD")
stock(symbols[1],currency="USD",multiplier=1)

signal[is.na(signal)]<-0


for(i in 2:length(signal))
{
	currentDate= time(signal)[i]
	equity = initEq #getEndEq(ltaccount, currentDate)
	position = getPosQty(ltportfolio, Symbol=symbols[1], Date=currentDate)	
	print(position)
	print(currentDate)
	if(position==0)
	{		
		#open a new position if signal is >0
		if(signal[i]>0 )
		{
			print('open position')
			closePrice<-as.double(Cl(SPY[currentDate]))
			print(closePrice)
			unitSize = as.numeric(trunc((equity/closePrice)))
			print(unitSize)
			commssions=-unitSize*closePrice*0.0003
			addTxn(ltportfolio, Symbol=symbols[1],  TxnDate=currentDate, TxnPrice=closePrice, TxnQty = unitSize , TxnFees=commssions, verbose=T)
			
		}
		
	}
	else
	{
		#position is open. If signal is 0 - close it.
		if(position>0 &signal[i] ==0)
		{
			position = getPosQty(ltportfolio, Symbol=symbols[1], Date=currentDate)
			closePrice<-as.double((Cl(SPY[currentDate])))#as.double(get(symbols[1])[i+100])
			commssions=-position*closePrice*0.0003
			addTxn(ltportfolio, Symbol=symbols[1],  TxnDate=currentDate, TxnPrice=closePrice, TxnQty = -position , TxnFees=commssions, verbose=T)
			
		}
		
	}	
	updatePortf(ltportfolio, Dates = currentDate)
	updateAcct(ltaccount, Dates = currentDate)
	updateEndEq(ltaccount, Dates = currentDate)
}
rez1<-(getPortfolio(ltaccount))
png('2highdays_2.png',width=650)
chart.CumReturns(rez1$symbols$SPY$txn[,7]/initEq)
dev.off()
