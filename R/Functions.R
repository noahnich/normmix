
## pnorm that allows for mixtures
pnormmix<-function(q,mean1,sd1,mean2,sd2,mixprob){
  F_X <- ((mixprob[1])*(pnorm(q, mean=mean1, sd=sd1)) + (mixprob[2])*pnorm(q, mean=mean2, sd=sd2))
  return(F_X)
}
dnormmix<-function(x,mean1,sd1,mean2,sd2,mixprob){
  f_X <- ((mixprob[1])*(dnorm(x, mean=mean1, sd=sd1)) + (mixprob[2])*dnorm(x, mean=mean2, sd=sd2))
  return(f_X)
}

## rnorm that allows for mixtures
rnormmix<-function(n,mean1,sd1,mean2,sd2,mixprob){
  mixture<-sample(1:2,n,prob=c(mixprob[1],mixprob[2]),replace=TRUE)
  iid<-rnorm(sum(mixture==1),mean=mean1,sd=sd1)
  iid2<-rnorm(sum(mixture==2),mean=mean2,sd=sd2)
  return(c(iid,iid2))
}

## qnorm that allows for mixtures
qnormmix<-function(p,mean1,sd1,mean2,sd2,mixprob){
  qmixp=Vectorize(function(p){
    my.fun<-function(x){pnormmix(x,mean1,sd1,mean2,sd2,mixprob)-p}
    results<-uniroot(my.fun, interval = c(-10,10), extendInt = "yes")
    return(results$root)
  })
  return(qmixp(p))
}