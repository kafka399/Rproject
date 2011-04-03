#Author: Dzidorius Martinaitis
#Date: 2011-04-4
#Description: http://www.investuotojas.eu/?p=471
require('ggplot2')

setwd('/home/git/Rproject/politika/')
partijos<-(read.table('savivaldybes.csv',header=TRUE,sep=','))

png('log_gyventojai_islaidos.png',width=650)
ggplot(partijos,aes(log(Gyventoju.skaicius),Islaidos))+geom_point(col='red',alpha=0.5,size=4)+geom_smooth()+ylab('Išlaidos')+xlab('Gyventojų sk. log. skalėje')
dev.off()

rez1<-droplevels(subset(partijos,partijos$Partija!=''))
levels(rez1$Partija)<-c("Dešinė","Centras","Dešinė","Kairė","Kairė","Centras","Centras","Dešinė","Centras")

png('kaire_desine_sk.png',width=650)
ggplot(rez1,aes(Partija,Gyventoju.skaicius,fill=Partija))+geom_bar(stat = "identity",alpha=0.9)+  scale_fill_manual(value=c("blue3","white","red3"))+opts(legend.position = "none")+  ylab('Gyventojų skaičius')+xlab('')
dev.off()


rez<-aggregate(partijos,list(partijos$Partija),length)[,1:2]
rez1<-subset(partijos,(partijos$Partija)%in% (rez[,1][rez[,2]>1]))
rez1<-droplevels(rez1)

png('gyv_sk_sum.png',width=650)
ggplot(rez1,aes(Partija,Gyventoju.skaicius,fill=Partija))+geom_bar(stat = "identity")+ylab('Gyventojų skaičius')
dev.off()

png('gyv_sk_taskai.png',width=650)
ggplot(rez1,aes(Partija,Gyventoju.skaicius/100000,col=Partija))+geom_jitter(size=4)+ylab('Gyventojų skaičius tūkst.')
dev.off()

png('islaidos_partijos.png',width=650)
ggplot(rez1,aes(Partija,Islaidos,col=Partija))+geom_jitter(size=4)+stat_summary(fun.y='mean',ymin=mean(rez1$Islaidos),ymax=mean(rez1$Islaidos),color='black',size=1.2)
dev.off()

png('islaidos_density.png',width=650)
ggplot(partijos,aes(Islaidos))+geom_density(col='blue4',fill='steelblue',size=1.2)+xlab('Išlaidos')
dev.off()
