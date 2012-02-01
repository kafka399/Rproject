#Author Dzidorius Martinaitis
#Date 2012-02-01
#Description http://www.investuotojas.eu/2012/02/01/vectorized-r-vs-rcpp

bid = runif(50000000,5,9)
ask = runif(50000000,5,9)
close = runif(50000000,5,9)

x=data.frame(bid=bid,ask=ask,last_price=close)
rez=0

###########    ifelse R  #################
answ=as.vector(system.time(
{
rez = ifelse(x$last_price>0,ifelse(x[, "bid"] > x[, "last_price"], x[, "bid"], ifelse((x[, "ask"] > 0) & (x[, "ask"] < x[, "last_price"]), x[, "ask"], x[, "last_price"])), 0.5*(x[, "ask"] + x[,"bid"]))
})[1])
###########   end ifelse R  #################

###########    vectorized R  #################

answ=append(answ,system.time(
{
lgt0 = x$last_price > 0
bgtl = x$bid > x$last_price
agt0 = x$ask > 0
altl = x$ask > x$last_price
rez = x$last_price
rez[lgt0 & agt0 & altl] = x$ask[lgt0 & agt0 & altl]
rez[lgt0 & bgtl] = x$bid[lgt0 & bgtl]
rez[!lgt0] = (x$ask[!lgt0]+x$bid[!lgt0])/2
}
)[1])
###########   end vectorized R  #################

#C++ code starts here

library(inline)
library(Rcpp)

###########    pure C++  #################

code='
NumericVector bid(bid_);NumericVector ask(ask_);NumericVector close(close_);NumericVector ret(ask_);
int bid_size = bid.size();
for(int i =0;i<bid_size;i++)
{
  if(close[i]>0)
  {
    if(bid[i]>close[i])
    {
      ret[i] = bid[i]; 
    }
    else if(ask[i]>0 && ask[i]<close[i])
    {
      ret[i] = ask[i];//
    }
    else
    {
      ret[i] = close[i];//
    }
  }
  else
  {
    ret[i]=(bid[i]+ask[i])/2;
  }
  
}
return ret;
'
getLastPrice <- cxxfunction(signature( bid_ = "numeric",ask_ = "numeric",close_="numeric"),body=code,plugin="Rcpp")
rez=0
answ=append(answ,system.time(
  {
    rez=getLastPrice(as.numeric(x$bid),as.numeric(x$ask),as.numeric(x$last_price))
  })[1])

###########   end pure C++  #################

#summary(rez)


###########    vectorized C++  #################
code='
NumericVector bid(bid_);NumericVector ask(ask_);NumericVector close(close_);NumericVector ret(ask_);
int bid_size = bid.size();
ret=ifelse(close>0,ifelse(bid >close, bid, ifelse(ask > 0,ifelse(ask < close,ask, close),close)), 0.5*(ask + bid));
return ret;
'
getLastPrice <- cxxfunction(signature( bid_ = "numeric",ask_ = "numeric",close_="numeric"),body=code,plugin="Rcpp")
rez=0
answ=append(answ,system.time(
{
  rez=getLastPrice(as.numeric(x$bid),as.numeric(x$ask),as.numeric(x$last_price))
}
)[1])

###########   end vectorized C++  #################

#summary(rez)
names(answ)=c('ifelse R','vectorized R','pure C++','vectorized C++')

library(ggplot2)
a=data.frame(ind=1:4,val=answ)
ggplot(a,aes(ind,val))+geom_point(legend=F)+geom_text(aes(label=names(answ),hjust=c(-0.2,-0.2,-0.2,0.8),vjust=c(0,0,0,-1)),size=4)