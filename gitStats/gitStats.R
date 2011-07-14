#Author: Dzidorius Martinaitis
#Date: 2011-07-13
#Description: http://www.investuotojas.eu/2011/07/13/plotting-git-statistics/

require(ggplot2)
require(xts)
setwd('/home/git/Rproject/gitStats/') 
Sys.setenv(TZ="GMT")
tmp=as.matrix(read.table('gitstats.csv',sep=',',header=FALSE))
commits=xts(cbind(as.double(tmp[,2]),as.double(tmp[,3]),as.double(tmp[,4])),order.by=as.POSIXct(strptime(tmp[,1],'%Y-%m-%d %H:%M:%S')))

colnames(commits)=c('Changes','Insertion','Deletion')
tmp=data.frame(Date=as.Date(index(commits)),Changes=as.numeric(commits$Changes),Insertion=as.numeric(commits$Insertion),Deletion=as.numeric(commits$Deletion))
tmp=melt(tmp,id.vars=c('Date'))
png('gitStats.png',width=500)
print(ggplot(tmp,aes(Date,value,color=variable))+geom_jitter(alpha=.65,size=3))
dev.off()

#############daily aggregated data##############
factor=as.factor(format(index(commits),'%Y%m%d'))
tmp=cbind(as.numeric(aggregate(commits$Changes,factor,sum)),as.numeric(aggregate(commits$Insertion,factor,sum)),as.numeric(aggregate(commits$Deletion,factor,sum)))
tmp=data.frame(unique(as.Date(index(commits))),tmp)
colnames(tmp)=c('Date','Changes','Insertion','Deletion')
tmp=melt(tmp,id.vars=c('Date'))
png('gitStats2.png',width=500)
print(ggplot(tmp,aes(Date,value,color=variable))+geom_jitter(alpha=.65,size=3))
dev.off()