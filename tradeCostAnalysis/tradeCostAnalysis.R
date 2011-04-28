Sys.setenv(TZ="GMT")
library(xts)
library(quantmod)
library("PerformanceAnalytics")
require('ggplot2')



###############################average daily volume###############################

avgDailyVolume<-function(x, window=100,plot=TRUE)
{
  factor<-as.factor(format(index(x),'%Y%m%d'))
  rez<-aggregate(Vo(x),factor,sum)
  rez<-as.xts(rez[,1],order.by=as.POSIXct(as.Date(strptime(as.character(index(rez)),'%Y%m%d'))))
  rez<-as.xts(rollapply(rez,window,mean,align='right'))
  if(plot==TRUE)
  {
     tmp<-data.frame(as.Date(index(rez)),round(rez[,1]))
     colnames(tmp)<-c('Date','Volume')
     subs<-tail(tmp,1)#subset(tmp,Date==last(tmp[,1]),Volume)
     p<-ggplot(tmp,aes(Date,Volume))+geom_line(color='steelblue',size=1.5)+geom_text(hjust = 0.7,
       vjust = 1,size = 3.5,col='red',aes(label=paste('Last: ',Volume)),data=subs);
     p<-p+scale_x_date(major='month',format='%Y-%m')+opts(title='Avg. daily volume');
    
     print(p);
  }
  return(last(rez))
}
#rez<-avgDailyVolume(trade,20)

volumeDensity<-function(x,q=0.05,plot=TRUE)
{
    factor<-as.factor(format(index(x),'%Y%m%d'))
    rez<-aggregate(Vo(x),factor,sum)
    rez<-as.xts(rez[,1],order.by=as.POSIXct(as.Date(strptime(as.character(index(rez)),'%Y%m%d'))))
    rez<-data.frame(Date=index(rez),Volume=rez[,1])
    if(plot==TRUE)
    {
      p<-ggplot(rez,aes(Volume))+geom_density(color='steelblue',size=1.5)+xlab('Volume')
      dens<-density(rez$Volume)
      rez1<-data.frame(x = dens$x, y = dens$y)
      
      p<-p+geom_area(data=subset(rez1,rez1$x<quantile(rez$Volume,q)),aes(x=x,y=y),fill = 'steelblue');
      print(p)
    }
    print(quantile(rez$Volume,0.05))
    return(rez)
}
#nasa<-volumeDensity(stock)

weeklyAvgVolume<-function(x,window=5,plot=TRUE)
{
  factor<-as.factor(format(index(x),'%Y%m%d'))
  dailyVolume<-aggregate(Vo(x),factor,sum)
  dailyVolume<-as.xts(dailyVolume[,1],order.by=as.POSIXct(as.Date(strptime(as.character(index(dailyVolume)),'%Y%m%d'))))
  dailyVolume[dailyVolume==0]<-NA
  dailyVolume<-na.approx(dailyVolume)
  factor<-as.factor(format(index(dailyVolume),'%w'))
  
  dailyAvr<-as.xts(rollapply(dailyVolume,window*5,median,align='right'))
  dailyVolume<-cbind(dailyVolume,dailyAvr)
  weekDayVolume<-0
  
 
  for(i in 1:length(levels(factor)))
  {
    temp<-as.xts(rollapply(dailyVolume[factor==levels(factor)[i]],window,
    function(z)
    {
     return(as.numeric(median(z[,1]))/(last(z[,2]))-1)
    },align='right',by.column = FALSE))
    
    if(i ==1)
      weekDayVolume<-temp
    else
    weekDayVolume<-rbind(weekDayVolume,temp)
  }
  
   factor<-as.factor(format(index(weekDayVolume),'%w'))
   result<-data.frame(as.Date(index(weekDayVolume)),(weekDayVolume[,1]),factor)
   
   colnames(result)<-c('Date','Volume','Factor')
   if(plot==TRUE)
   {
     #p<-ggplot(result,aes(Date,Volume,col=Factor))+geom_line(size=1.5)+opts(title='Day of the week volatility')
     #print(p)
     #readline(prompt = "Pause. Press <Enter> to continue...")
     p<-ggplot(result,aes(Factor,Volume,col=Factor))+xlab('Weekday')+geom_boxplot()+opts(title='Day of the week volume');
     print(p)
   }
 #  print(anova(lm(as.numeric(Factor)~Volume,result)))
   #return(anova(lm(as.numeric(Factor)~Volume,result)))
   return(result)
}
#nasa<-weeklyAvgVolume(trade,10)


weekdayDensity<-function(x,window=5,plot=TRUE)
{
  factor<-as.factor(format(index(x),'%Y%m%d'))
  dailyVolume<-aggregate(Vo(x),factor,sum)
  dailyVolume<-as.xts(dailyVolume[,1],order.by=as.POSIXct(as.Date(strptime(as.character(index(dailyVolume)),'%Y%m%d'))))
  dailyVolume[dailyVolume==0]<-NA
  dailyVolume<-na.approx(dailyVolume)
  factor<-as.factor(format(index(dailyVolume),'%w'))
  
  dailyAvr<-as.xts(rollapply(dailyVolume,window*5,median,align='right'))
  dailyVolume<-cbind(dailyVolume,dailyAvr)
  weekDayVolume<-0
  
 
  for(i in 1:length(levels(factor)))
  {
    temp<-as.xts(rollapply(dailyVolume[factor==levels(factor)[i]],window,
    function(z)
    {
     return(as.numeric(median(z[,1]))/(last(z[,2]))-1)
    },align='right',by.column = FALSE))
    
    if(i ==1)
      weekDayVolume<-temp
    else
    weekDayVolume<-rbind(weekDayVolume,temp)
  }
  
   factor<-as.factor(format(index(weekDayVolume),'%w'))
   result<-data.frame(as.Date(index(weekDayVolume)),(weekDayVolume[,1]),factor)
   
   colnames(result)<-c('Date','Volume','Factor')
   if(plot==TRUE)
   {
     p<-ggplot(result,aes(Volume,fill=Factor))+geom_density(alpha=0.5, size=1.1)
     print(p)
   }
 #  print(anova(lm(as.numeric(Factor)~Volume,result)))
   #return(anova(lm(as.numeric(Factor)~Volume,result)))
   return(result)
}
###########intraday################
################min volatility#####################
minutesVol<-function(x,plot=TRUE)
{
  delta<-Delt(Cl(to.minutes5(Cl(x))),type='log')
  factor<-as.factor(format(index(delta),'%Y%m%d%H'))
  data<-aggregate((delta),factor,sd)  
  data<-as.xts(data[,1],order.by=as.POSIXct((strptime(index(data),'%Y%m%d%H'))))
  data<-data[!is.na(data)]
  factor<-as.factor(format(index(data),'%H'))
  data<-data.frame(as.numeric(data),factor)
  colnames(data)<-c('Delta','Hour')
  
  if(plot==TRUE)
  {
    p<-ggplot(data,aes(Hour,Delta,col=Hour))+geom_jitter(alpha=.65)+opts(title='Avg. volatility per hour')+
    stat_summary(fun.y='mean',ymin=mean(data$Delta),ymax=mean(data$Delta),color='black')
    print(p)
  }
  data<-aggregate(data$Delta,list(hour=(data$Hour)),median)
  return(data)
}
#nasa<-minutesVol(x)


##################average trade##########################
avgTradeVolume<-function(x,plot=TRUE)
{
  data<-x[which(Vo(x)!=0)]
  factor<-as.factor(format(index(data),'%H'))
  rez<-aggregate(Vo(data),factor,median)
  data<-data.frame(as.numeric(Vo(data)),factor)
  colnames(data)<-c('Volume','Hour')
  
  if(plot==TRUE)
  {
    p<-ggplot(data,aes(Hour,Volume,col=Hour))+geom_jitter(alpha=.45)+opts(title='Avg. trade per hour')+
    stat_summary(fun.y='median',ymin=median(data$Volume),ymax=median(data$Volume),color='black')
    print(p)
  }
  return(rez)
}
##################end of average trade##########################
#nasa<-avgTradeVolume(stock)


##################cumulative Volume per hour##########################
hourlyVolume<-function(x,plot=TRUE)
{
  data<-x[which(Vo(x)!=0)]
  factor<-as.factor(format(index(data),'%Y-%m-%d %H'))
  data<-aggregate(Vo(data),factor,sum)
  data<-as.xts(data[,1],order.by=as.POSIXct((strptime(as.character(index(data)),'%Y-%m-%d %H'))))
  
  
  
  factor<-as.factor(format(index(data),'%H'))
  data<-data.frame(as.numeric(data),factor)
  colnames(data)<-c('Volume','Hour')
  
  if(plot==TRUE)
  {
    p<-ggplot(data,aes(Hour,Volume,col=Hour))+geom_jitter(alpha=.65)+
    stat_summary(fun.y='median',ymin=median(data$Volume),ymax=median(data$Volume),color='black')+opts(title='Volume per hour')
    print(p)
  }
  data<-aggregate(data$Volume,list(hour=(data$Hour)),median)
  return(data)
}
##################end of average trade##########################
#nasa<-hourlyVolume(stock)

volumeStability<-function(x,plot=TRUE)
{
  factor<-as.factor(format(index(x),'%Y%m%d'))
  rez<-aggregate(Vo(x),factor,sum)
  rez<-as.xts(rez[,1],order.by=as.POSIXct(as.Date(strptime(as.character(index(rez)),'%Y%m%d'))))
  data<-data.frame(index(rez),rez[,1])
  rez<-(sd(rez))/mean(rez)
  colnames(data)<-c('Date','Volume')
  if(plot==TRUE)
  {
    p<-ggplot(data,aes(Date,Volume))+geom_line(size=1.5,col='steelblue')+geom_smooth(col='red',size=1.2)+opts(title='Volume stability')
    print(p)
  }
  
  return(rez)
}
#nasa<-volumeStability(trade)