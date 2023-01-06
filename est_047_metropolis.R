# Gerar 100000 amostras de uma gama(1,b)

x=rep(0,10^5)
x[1]=1

for(k in 1:99999){
  v=x[k]+rnorm(1,sd=5)
  u=runif(1)
    if(u<dgamma(v,shape = 2)/dgamma(x[k],shape=2)){
      x[k+1]=v
    }
      else{
        x[k+1]=x[k]
      }

}

hist(x,breaks=100)


#2


x=matrix(0,nrow = 10000,ncol=2)
x[1,]=c(1,1)
d=matrix(c(1,.25,.25,1),nrow=2,ncol=2)
d_1=solve(d)
for(k in 1:9999){
  v=x[k,]+rnorm(2,sd=0.01)
  u=runif(1)

  d_v=(1+1/15*v%*%d_1%*%v)^(-17/2)
  d_x=(1+1/15*x[k,]%*%d_1%*%x[k,])^(-17/2)

  if(u<d_v/d_x){
    x[k+1,]=v
  }
  else{
    x[k+1,]=x[k,]
  }

}


plot(x,type='l')
