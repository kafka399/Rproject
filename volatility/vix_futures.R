Sys.setenv(TZ="GMT")
require('xts')
require('quantmod')
require('blotter')
require('PerformanceAnalytics')

tmp<-as.matrix(read.table('tickers/various_day_close/VIXc1.csv',sep=',',header=TRUE))
vix<-as.xts(as.double(tmp[,9]),order.by=as.POSIXct(strptime(tmp[,2],'%d-%b-%Y'),tz='GMT'))
vix<-(vix[!is.na(vix)])
colnames(vix)<-c('Close')

tmp<-as.matrix(read.table('tickers/various_day_close/VIXc2.csv',sep=',',header=TRUE))
vix2<-as.xts(as.double(tmp[,9]),order.by=as.POSIXct(strptime(tmp[,2],'%d-%b-%Y')))
vix2<-(vix2[!is.na(vix2)])
colnames(vix2)<-c('Close')


#-----------------data end-----------------


#-----------------signal-------------------

delta<-Delt(Cl(vix))#Front contract

short.vol<-as.xts(rollapply(delta,3,sd,align='right'))
long.vol<-as.xts(rollapply(delta,10,sd,align='right'))

past.vol<-short.vol/long.vol

signal<-ifelse(past.vol<0.25,1,0)
signal<-signal[index(vix)]
#-----------------signal end-------------------

#--------------blotter code------------------
symbols<-c('vix')

initDate=time(get(symbols)[1])
initEq=15000
rm(list=ls(envir=.blotter),envir=.blotter)
ltportfolio='volatility'
ltaccount='volatility'
initPortf(ltportfolio,symbols, initDate=initDate)
initAcct(ltaccount,portfolios=c(ltportfolio), initDate=initDate,initEq=initEq)
currency("USD")
stock(symbols[1],currency="USD",multiplier=1)

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
			unitSize = as.numeric(trunc((equity/closePrice)))
			print(unitSize)
			commssions=-unitSize*closePrice*0.0003
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
			commssions=-position*closePrice*0.0003
			addTxn(ltportfolio, Symbol=symbols[1],  TxnDate=currentDate, TxnPrice=closePrice, TxnQty = -position , TxnFees=commssions, verbose=T)
			counter<-0
		}
		else
			counter<-counter+1
		
	}	
	print('>>>>>>>>>>>>')
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

#-------------------data preparation, merge with VXX ETF-----------------

tmp<-as.matrix(read.table('tickers/various_day_close/VIXc1.csv',sep=',',header=TRUE))
vix<-as.xts(as.double(tmp[,9]),order.by=as.POSIXct(strptime(tmp[,2],'%d-%b-%Y'),tz='GMT'))
vix<-(vix[!is.na(vix)])
colnames(vix)<-c('Close')

getSymbols('VXX')

initDate<-index(last(vix))

VXX<-Ad(VXX[paste(initDate,'::',sep='')])
delta<-Delt(vix)
delta<-(rbind(tail(delta,-1),tail(Delt(VXX),-1)))

vix<-cumprod(delta+1)
colnames(vix)<-c('Close')

#------------------------signal---------------------------
short.vol<-as.xts(rollapply(vix,3,sd,align='right'))
long.vol<-as.xts(rollapply(vix,10,sd,align='right'))

past.vol<-short.vol/long.vol

signal<-ifelse(past.vol<0.25,1,0)
signal<-signal[index(vix)]



#------------------------blotter code-----------------------
symbols<-c('vix')

initDate=time(get(symbols)[1])
initEq=15000
rm(list=ls(envir=.blotter),envir=.blotter)
ltportfolio='volatility'
ltaccount='volatility'
initPortf(ltportfolio,symbols, initDate=initDate)
initAcct(ltaccount,portfolios=c(ltportfolio), initDate=initDate,initEq=initEq)
currency("USD")
stock(symbols[1],currency="USD",multiplier=1)

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
			unitSize = as.numeric(trunc((equity/closePrice)))
			print(unitSize)
			commssions=-unitSize*closePrice*0.0003
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
			commssions=-position*closePrice*0.0003
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
png('vix_merged.png',width=650)
chart.TimeSeries(cumsum(rez1$symbols$vix$txn[,7]),main='VIX front merged with VXX')
dev.off()

#--------------------------------ONLY VXX--------------------------------


getSymbols('VXX')
VXX<-Ad(VXX)
colnames(VXX)<-c('Close')

#------------------------signal---------------------------
short.vol<-as.xts(rollapply(VXX,3,sd,align='right'))
long.vol<-as.xts(rollapply(VXX,10,sd,align='right'))

past.vol<-short.vol/long.vol

signal<-ifelse(past.vol<0.25,1,0)
signal<-signal[index(VXX)]



#------------------------blotter code-----------------------
symbols<-c('VXX')

initDate=time(get(symbols)[1])
initEq=15000
rm(list=ls(envir=.blotter),envir=.blotter)
ltportfolio='volatility'
ltaccount='volatility'
initPortf(ltportfolio,symbols, initDate=initDate)
initAcct(ltaccount,portfolios=c(ltportfolio), initDate=initDate,initEq=initEq)
currency("USD")
stock(symbols[1],currency="USD",multiplier=1)

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
			unitSize = as.numeric(trunc((equity/closePrice)))
			print(unitSize)
			commssions=-unitSize*closePrice*0.0003
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
			commssions=-position*closePrice*0.0003
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
png('vxx.png',width=650)
chart.TimeSeries(cumsum(rez1$symbols$VXX$txn[,7]),main='VXX')
dev.off()

#--------------------------------END ONLY VXX--------------------------------
