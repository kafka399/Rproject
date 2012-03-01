#Author Dzidorius Martinaitis
#Date 2012-03-01
#Description http://www.investuotojas.eu/2012/03/01/i-see-high-frequency-data/

require(rmongodb)
require(xts)
require(ggplot2)
mongo<-mongo.create()

buf <- mongo.bson.buffer.create()
mongo.bson.buffer.append(buf, "tickerId", 20L)
mongo.bson.buffer.start.object(buf, "size")
mongo.bson.buffer.append(buf, "$exists", "true")
mongo.bson.buffer.finish.object(buf)

query <- mongo.bson.from.buffer(buf)

count = mongo.count(mongo,'quotes.trinti',query)
cursor<-mongo.find(mongo,'quotes.trinti',query)

#############  very slow code #############
#size=''
#system.time(
#while(mongo.cursor.next(cursor)){
#  temp=(mongo.cursor.value(cursor));
#  if(is.xts(size))
#    size=rbind(size,xts(cbind(mongo.bson.value(temp,"field"),mongo.bson.value(temp,"size")),order.by=as.POSIXct(mongo.bson.value(temp,"tstamp")/1000,origin='1970-01-01',tz='Europa/Paris')))
#  else
#    size=xts(cbind(mongo.bson.value(temp,"field"),mongo.bson.value(temp,"size")),order.by=as.POSIXct(mongo.bson.value(temp,"tstamp")/1000,origin='1970-01-01',tz='Europa/Paris'))  
})
#############  end very slow  #############

size=matrix(nrow=count,ncol=3)
counter=1
system.time(
  while(mongo.cursor.next(cursor))
  {
    temp=(mongo.cursor.value(cursor));
    size[counter,1]=mongo.bson.value(temp,"field");
    size[counter,2]=mongo.bson.value(temp,"size");
    size[counter,3]=mongo.bson.value(temp,"tstamp");
    counter=counter+1;
    if(counter>count)break;
    })
size=xts(size[,1:2],order.by=as.POSIXct(size[,3]/1000,origin='1970-01-01',tz='Europe/Paris'))
colnames(size)=c('field','size')


buf <- mongo.bson.buffer.create()
mongo.bson.buffer.append(buf, "tickerId", 26L)
mongo.bson.buffer.start.object(buf, "price")
mongo.bson.buffer.append(buf, "$exists", "true")
mongo.bson.buffer.finish.object(buf)

query <- mongo.bson.from.buffer(buf)
count = mongo.count(mongo,'quotes.trinti',query)

cursor<-mongo.find(mongo,'quotes.trinti',query)
price=matrix(nrow=count,ncol=3)
counter=1
system.time(
  while(mongo.cursor.next(cursor))
  {
    temp=(mongo.cursor.value(cursor));
    price[counter,1]=mongo.bson.value(temp,"field");
    price[counter,2]=mongo.bson.value(temp,"price");
    price[counter,3]=mongo.bson.value(temp,"tstamp");
    counter=counter+1;
    if(counter>count)break;
  })
price=xts(price[,1:2],order.by=as.POSIXct(price[,3]/1000,origin='1970-01-01',tz='Europe/Paris'))
price=(price[which(price[,2]>0)])

colnames(price)=c('field','price')

quotes<-cbind(price[,2][price[,1]==1],
             #cac40.volume[,2][cac40.volume[,1]==0],
             price[,2][price[,1]==2],
             #cac40.volume[,2][cac40.volume[,1]==3],
             price[,2][price[,1]==4]
             ,size[,2][size[,1]==5]
             )

quotes[,1]=na.locf(quotes[,1])
quotes[,2]=na.locf(quotes[,2])
quotes[,3]=na.locf(quotes[,3])
quotes[which(is.na(quotes[,4])),3]=NA

temp=tail(head(quotes,3000),400)
temp=data.frame(ind=1:NROW(temp),trd=as.numeric(temp[,3])                
                ,bid=as.numeric(temp[,1]),ask=as.numeric(temp[,2])
                ,size=as.numeric(temp[,4])
                )
temp=melt(temp,id=c('ind'),na.rm=TRUE)
x=temp[which(temp$variable=='trd'),]

rez=temp[which(temp$variable!='trd'),]
rez=rez[which(rez$variable!='size'),]
a=temp[which(temp$variable=='size'),][,3]
ggplot(rez,aes(x=ind,y=value,color=variable))+geom_line()+geom_point(data=x,aes(size=a))