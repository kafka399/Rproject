#Author Dzidorius Martinaitis
#2011-01-21
#Description: 
#status: case closed

Sys.setenv(TZ="GMT")
require('xts')
require('quantmod')
require('blotter')
require('PerformanceAnalytics')

tmp<-as.matrix(read.table('tickers/various_day_close/VIXc1.csv',sep=',',header=TRUE))
vix<-as.xts(as.double(tmp[,9]),order.by=as.POSIXct(strptime(tmp[,2],'%d-%b-%Y'),tz='GMT'))
vix<-(vix[!is.na(vix)])
colnames(vix)<-c('Close')


tmp<-as.matrix(read.table('tickers/various_day_close/ESc1.csv',sep=',',header=TRUE))
es<-as.xts(as.double(tmp[,9]),order.by=as.POSIXct(strptime(tmp[,2],'%d-%b-%Y')))
es<-(es[!is.na(es)])
colnames(es)<-c('Close')

#-----------------data end-----------------


#-----------------signal-------------------
es.delta<-Delt(Cl(es))
delta<-Delt(Cl(vix))#Front contract

#Historical volatility during 3 and 10 days
short.vol<-as.xts(rollapply(es.delta,3,sd,align='right'))
long.vol<-as.xts(rollapply(es.delta,10,sd,align='right'))

past.vol<-short.vol/long.vol
future.vol<-lag(past.vol,-3)
future.delta<-lag(vix,-3)/vix-1

signal<-ifelse(past.vol<0.25,1,0)

#here we see, increase in historical volatility
summary(as.double(future.vol[index(signal[signal!=0])]))/summary(as.double(past.vol[index(signal[signal!=0])]))

#-----------------signal end-------------------

#--------------blotter code------------------
symbols<-c('vix')

initDate=time(get(symbols)[1])
initEq=50000
rm(list=ls(envir=.blotter),envir=.blotter)
ltportfolio='volatility'
ltaccount='volatility'
initPortf(ltportfolio,symbols, initDate=initDate)
initAcct(ltaccount,portfolios=c(ltportfolio), initDate=initDate,initEq=initEq)
currency("USD")
future(symbols[1],currency="USD",1000,0.01)

#signal<-signal[index(vix)]
		
signal[is.na(signal)]<-0

counter<-0 #date counter - exit on 3th day

for(i in 2:length(signal))
{
	currentDate= time(signal)[i]
	equity = initEq #getEndEq(ltaccount, currentDate)
	position = getPosQty(ltportfolio, Symbol=symbols[1], Date=currentDate)	
	print(position)
	print(currentDate)
	if(position==0 &counter==0)
	{		
		#open a new position if signal is >0
		if(signal[i]>0)
		{
			print('open position')
			closePrice<-as.double(get(symbols[1])[currentDate])
			print(closePrice)
			unitSize = 1#as.numeric(trunc((equity/closePrice)))
			print(unitSize)
			commssions=-2#unitSize*closePrice*0.0003
			addTxn(ltportfolio, Symbol=symbols[1],  TxnDate=currentDate, TxnPrice=closePrice-0.02, TxnQty = unitSize , TxnFees=commssions, verbose=T)
			counter=1
		}
		
	}
	else
	{
		#position is open. If signal is 0 - close it.
		if(position!=0 & as.integer(signal[i])==0 &counter>=10)
		{
			position = getPosQty(ltportfolio, Symbol=symbols[1], Date=currentDate)
			closePrice<-as.double(get(symbols[1])[currentDate])#as.double(get(symbols[1])[i+100])
			commssions=-2#position*closePrice*0.0003
			addTxn(ltportfolio, Symbol=symbols[1],  TxnDate=currentDate, TxnPrice=closePrice+0.02, TxnQty = -position , TxnFees=commssions, verbose=T)
			counter<-0
		}
		else
			counter<-counter+1
		
	}	
	print(paste('>>>>>>>>>>>>',counter))
	updatePortf(ltportfolio, Dates = currentDate)
	updateAcct(ltaccount, Dates = currentDate)
	updateEndEq(ltaccount, Dates = currentDate)
}
rez1<-(getPortfolio(ltaccount))

#--------------blotter code end------------------

#----------------results------------------------
png('vix_front.png',width=650)
#net profit - commissions, slipage excluded
chart.TimeSeries(cumsum(rez1$symbols$vix$txn[,7]),main='VIX front contract')
dev.off()
#----------------results end------------------------

###################################################################################################

signal<-ifelse(past.vol<0.25,1,0)
#signal<-signal[index(es)]



#------------------------blotter code-----------------------
symbols<-c('es')

initDate=time(get(symbols)[1])
initEq=15000
rm(list=ls(envir=.blotter),envir=.blotter)
ltportfolio='volatility'
ltaccount='volatility'
initPortf(ltportfolio,symbols, initDate=initDate)
initAcct(ltaccount,portfolios=c(ltportfolio), initDate=initDate,initEq=initEq)
currency("USD")
future(symbols[1],currency="USD",multiplier=50,1/4)

signal[is.na(signal)]<-0

counter<-0

for(i in 2:length(signal))
{
	currentDate= time(signal)[i]
	equity = initEq #getEndEq(ltaccount, currentDate)
	position = getPosQty(ltportfolio, Symbol=symbols[1], Date=currentDate)	
	print(position)
	print(currentDate)
	if(position==0 &counter==0)
	{		
		#open a new position if signal is >0
		if(signal[i]>0)
		{
			print('open position')
			closePrice<-as.double(get(symbols[1])[currentDate])
			print(closePrice)
			unitSize = 1#as.numeric(trunc((equity/closePrice)))
			print(unitSize)
			commssions=-2
			addTxn(ltportfolio, Symbol=symbols[1],  TxnDate=currentDate, TxnPrice=closePrice, TxnQty = unitSize , TxnFees=commssions, verbose=T)
			counter<-1
		}
		
	}
	else
	{
		#position is open. If signal is 0 - close it.
		if(position>0 & as.integer(signal[i])==0 &counter>=3)
		{
			position = getPosQty(ltportfolio, Symbol=symbols[1], Date=currentDate)
			closePrice<-as.double(get(symbols[1])[currentDate])#as.double(get(symbols[1])[i+100])
			commssions=-2
			addTxn(ltportfolio, Symbol=symbols[1],  TxnDate=currentDate, TxnPrice=closePrice, TxnQty = -position , TxnFees=commssions, verbose=T)
			counter<-0
		}
		else
			counter<-counter+1
		
	}	

	updatePortf(ltportfolio, Dates = currentDate)
	updateAcct(ltaccount, Dates = currentDate)
	updateEndEq(ltaccount, Dates = currentDate)
}
rez1<-(getPortfolio(ltaccount))
#-------------------------results---------------------
#net profit
png('vix.png',width=650)
chart.TimeSeries(cumsum(rez1$symbols$es$txn[,9]),main='ES future contract')
dev.off()
