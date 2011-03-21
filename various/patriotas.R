# result: http://dl.dropbox.com/u/6360678/Rproject_data/patriotai.png
#@Autor Dzidorius Martinaitis
#@Date 2011-03-21
library(RColorBrewer)
library(sp)
#http://gadm.org/data/rda/LTU_adm1.RData
#getData("GADM",country='LTU',level=2)#<-doesn't work on my machine
load(file='~/tickers/LTU_adm2.RData')

patriotas<-tail(read.table('http://dl.dropbox.com/u/6360678/Rproject_data/patriotas.csv',sep=',',header=T),-1)
head(patriotas)
factor<-as.factor(cut(as.numeric(sub(',','.',patriotas$X)),c(30,40,50,60,70)))

gadm$col_no<-factor

png('patriotai.png',width=500)
spplot(gadm, "col_no", col=grey(.9), col.regions=brewer.pal(4,"Purples"),main='Patriotai Lietuvoje')
dev.off()