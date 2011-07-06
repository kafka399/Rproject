#Author Dzidorius Martinaitis
#Date 2011-07-06
#Description http://www.investuotojas.eu/?p=500

setwd('/home/git/Rproject/kmeans/')
require(quantmod)
require(ggplot2)
Sys.setenv(TZ="GMT")
getSymbols('SPY',from='2000-01-01')


x=data.frame(d=index(Cl(SPY)),return=as.numeric(Delt(Cl(SPY))))
png('daily_density.png',width=500)
ggplot(x,aes(return))+stat_density(colour="steelblue", size=2, fill=NA)+xlab(label='Daily returns')
dev.off()

nasa=tail(cbind(Delt(Op(SPY),Hi(SPY)),Delt(Op(SPY),Lo(SPY)),Delt(Op(SPY),Cl(SPY))),-1)

###################if you want uniform distribution - uncomment the following lines##############
#rez=apply(
#  (nasa),2,function(x){
#    y=(xts(x,order.by=as.POSIXct(as.Date(attr(x,'names')))))
#    
#    print(xts(rollapply(y,20,function(z){which(order(z)==20)/20},align='right')))
#    }  
#)
#nasa=xts(rez,order.by=as.POSIXct(tail(index(nasa),-19)))
#colnames(nasa)=c('High','Low','Close')
#######################uniform distribution END##############################

#optimal number of clusters
wss = (nrow(nasa)-1)*sum(apply(nasa,2,var))
for (i in 2:15) wss[i] = sum(kmeans(nasa, centers=i)$withinss)
wss=(data.frame(number=1:15,value=as.numeric(wss)))

png('numberOfClusters.png',width=500)
ggplot(wss,aes(number,value))+geom_point()+
  xlab("Number of Clusters")+ylab("Within groups sum of squares")+geom_smooth()
dev.off()

kmeanObject=kmeans(nasa,5,iter.max=10)
kmeanObject$centers
autocorrelation=head(cbind(kmeanObject$cluster,lag(as.xts(kmeanObject$cluster),-1)),-1)
xtabs(~autocorrelation[,1]+(autocorrelation[,2]))

y=apply(xtabs(~autocorrelation[,1]+(autocorrelation[,2])),1,sum)
x=xtabs(~autocorrelation[,1]+(autocorrelation[,2]))
z=x
for(i in 1:5)
{
  z[i,]=(x[i,]/y[i])
}
round(z,2)

