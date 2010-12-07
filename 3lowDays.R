require('xts')
require('quantmod')
require('blotter')
require('PerformanceAnalytics')
require('FinancialInstrument')
Sys.setenv(TZ="GMT")

getSymbols('SPY',from='1995-01-01',index.class=c("POSIXt","POSIXct"))
dividends<-getDividends('SPY',from='1995-01-01',index.class=c("POSIXt","POSIXct"))

temp<-cbind(dividends,SPY)
temp[,1][is.na(temp[,1])]<-0

SPY<-cbind(temp[,2],temp[,3],temp[,4],temp[,1]+temp[,5])
colnames(SPY)<-c('Open','High','Low','Close')

#one day before
lag1<-lag((SPY),1)

#two days defore
lag2<-lag((SPY),2)

signal<-ifelse( (Cl(lag2)>Cl(lag1) & Cl(lag1)>Cl(SPY))&
			(Hi(lag2)>Hi(lag1) & Hi(lag1)>Hi(SPY)) &
			(Op(lag2)>Op(lag1) & Op(lag1)>Op(SPY)),
			1,0
)
#one day later
lag3<-lag(Cl(SPY),-1)


profit<-(lag3/Cl(SPY)-1)*signal
chart.CumReturns(profit)

#------------------
tmp<-(Hi(lag2)/Lo(SPY)-1)*signal

#blotter code
symbols<-c('SPY')

initDate=time(get(symbols)[1])
initEq=10000
rm(list=ls(envir=.blotter),envir=.blotter)
ltportfolio='3days'
ltaccount='3days'
initPortf(ltportfolio,symbols, initDate=initDate)
#initPortf(shportfolio,symbols, initDate=initDate)
initAcct(ltaccount,portfolios=c(ltportfolio), initDate=initDate,initEq=initEq)
currency("USD")
stock(symbol,currency="USD",multiplier=1)

signal[is.na(signal)]<-0

#tmp<-ifelse(SMA(Cl(SPY))<Cl(SPY),1,0)
#tmp[is.na(tmp)]<-0
#signal<-signal*tmp

counter<-0

for(i in 10:length(signal))
{
	currentDate= time(signal)[i]
	equity = 10000 #getEndEq(ltaccount, currentDate)
	#print(paste("equity ",equity))
	position = getPosQty(ltportfolio, Symbol=symbols[1], Date=currentDate)	
	print(position)
	print(currentDate)
	if(position==0)
	{		
		#open a new position if signal is >0
		if(signal[i]>0 &counter ==0)
		{
			print('open position')
			closePrice<-as.double(Cl(SPY[currentDate]))
			print(closePrice)
			unitSize = as.numeric(trunc((equity/closePrice)))
			print(unitSize)
			commssions=unitSize*closePrice*0.0003
			addTxn(ltportfolio, Symbol=symbols[1],  TxnDate=currentDate, TxnPrice=closePrice, TxnQty = unitSize , TxnFees=commssions, verbose=T)
			counter<-1
		}
		
	}
	else
	{
		#position is open. If signal is 0 - close it.
		if(position>0 &counter>=1)
		{
			print('close position>>>>')
			position = getPosQty(ltportfolio, Symbol=symbols[1], Date=currentDate)
			closePrice<-as.double((Cl(SPY[currentDate])))#as.double(get(symbols[1])[i+100])
			commssions=-position*closePrice*0.0003
			addTxn(ltportfolio, Symbol=symbols[1],  TxnDate=currentDate, TxnPrice=closePrice, TxnQty = -position , TxnFees=commssions, verbose=T)
			counter<-0
		}
		else
		{
			print(counter)
			counter<-counter+1
		}
	}	
	#print(equity)
	updatePortf(ltportfolio, Dates = currentDate)
	updateAcct(ltaccount, Dates = currentDate)
	updateEndEq(ltaccount, Dates = currentDate)
	
	#equity = getEndEq(ltaccount, currentDate)
	#print(paste("equity ",equity))
}
rez1<-(getPortfolio(ltaccount))
plot(cumsum(rez1$symbols$SPY$txn[,9]))

#gross profit
result<-rez1$symbols$SPY$txn[,7]
result<-result[result!=0]

#fix commission rate 2*3
plot(cumsum(result-6))

#chart.CumReturns(cbind((result)/10000,(result-6)/10000))

