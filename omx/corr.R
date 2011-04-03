#@Author: Dzidorius Martinaitis
#@Date: 2011-03-22
#@Description: http://www.investuotojas.eu/?p=464

require(xts)
require(quantmod)
loadOMX<-function(path, name)
{
        
        filename<-paste(path,sep='')
        print(filename)
        temp<-as.matrix(read.table(file=filename,sep=',',quote = "",header=TRUE))
        
        #result[counter]<-temp
        graph<-as.POSIXct(as.Date(temp[,3],'%Y%m%d'),tz='GMT')
        tempxts<-xts(as.double(temp[,8]),order.by=graph)
        
        tempxts<-tempxts[!is.na(tempxts)]
        
        colnames(tempxts)<-c('Close')
        
        if(length(name)>0)
        {
                print(name)
                assign(name, tempxts,envir = .GlobalEnv)
        }
        #print(tail(tempxts))
        return(tempxts)
        
}
mergeData<-function(tickers)
{
        mergedList<-''
        #print(length(tickers))
        for (counter in 1:length(tickers))
        {
                print(tickers[counter])
                if(counter == 1)
                {
                        mergedList=Cl(get(tickers[counter]))    
                }               else
                {
                        mergedList=cbind(mergedList,Cl(get(tickers[counter])))
                }
        }
        
        #colnames(mergedList)<-tickers
        return(mergedList)
}

tickers<-c('TEO1L', 'IVL1L','APG1L','SRS1L','UKB1L', 'SAB1L','LDJ1L','GRG1L','KNF1L','CTS1L',
'RSU1L','PZV1L','DKR1L','KBL1L','STU1L','SNG1L','UTR1L','VBL1L','SAN1L','PTR1L','LFO1L')

for (counter in 1:length(tickers))
{
#       print(tickers[counter])
        loadOMX(paste('~/tickers/qoutes/',tickers[counter],'.txt',sep=''),tickers[counter])
}

#merge all tickers into one
tmp<-mergeData(tickers)
colnames(tmp)<-(tickers)


cor_mat<-matrix(ncol=NCOL(tmp),nrow=NCOL(tmp))
colnames(cor_mat)<-tickers
rownames(cor_mat)<-tickers

for (i in 1:NCOL(tmp))
{
  for (y in 1:NCOL(tmp))
  {
      if(i!=y)
      {
         cor_mat[i,y]<-cor(tail(tail(na.locf(tmp[,i])['2010::']/lag(na.locf(tmp[,i])['2010::'],1)-1,-1),60),tail(tail(na.locf(tmp[,y])['2010::']/lag(na.locf(tmp[,y])['2010::'],1)-1,-1),60))
      }
      else
      {
         cor_mat[i,y]<-0
      }
  }
}

require(igraph)
#cor_mat<- matrix( runif(100), nr=10 )
cor_mat[ lower.tri(cor_mat, diag=TRUE) ]<- 0
cor_mat[ abs(cor_mat) < 0.5] <- 0

graph <- graph.adjacency(cor_mat>0.5, weighted=TRUE, mode="upper")
E(graph)$weight<-t(cor_mat)[abs(t(cor_mat))>0.5]
E(graph)[ weight>0.7 ]$color <- "black"
E(graph)[ weight>=0.65 & weight<0.7 ]$color <- "red"
E(graph)[ weight>=0.6 &weight<0.65 ]$color <- "green"
E(graph)[ weight>=0.55 &weight<0.6 ]$color <- "blue"
E(graph)[ weight<0.55  ]$color <- "yellow"
V(graph)$label<- V(graph)$name
graph$layout <- layout.fruchterman.reingold
factor<-as.factor(cut(E(graph)$weight*10,c(4,5,6,7,8),labels=c(1,10,20,30)))
V(graph)$size<-degree(graph)
png('corr_network.png',width=500)
plot(decompose.graph(graph)[[which.max(sapply(decompose.graph(graph), vcount))]],edge.width =as.numeric(factor)*1.5,frame=T)
legend("bottomleft", title="Colors", cex=0.75, pch=16, col=c("black", "blue","red", "green","pink"), legend=c(">70%", "65-70","60-65","55-60","50-55"), ncol=2)
dev.off()