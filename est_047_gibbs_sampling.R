# Dado (X1,X2)~N(0,sigma), onde sigma eh a matriz de covariancia

x=matrix(0,nrow=10000,ncol=2)
x[1,]=c(1,1)

for(k in 1:9999){
  x[k+1,1]=rnorm(1,0.9*x[k,2],sd=sqrt(1-0.9^2))
  x[k+1,2]=rnorm(1,0.9*x[k+1,1],sd=sqrt(1-0.9^2))
}





# 2. (mu, sigma²)

x=matrix(0,nrow=10000,ncol=2)
mu=rep(0,10000)
sig=rep(0,10000)

for(k in 1:9999){
  # mu[k+1]=rnorm(1,media,var=sigma^2/n)
  mu[k+1]=rnorm(1,20,sd=sqrt(4/10))
  # sig[k+1]=rgamma(1,n/2,(n(x_barra-mu[k+1])^2+n*sigma^2)/2)^(-1)
  sig[k+1]=rgamma(1,shape=10/2,rate=(10*(20-mu[k+1])^2+10*4)/2)^(-1)
}

#3. Autoexponencial bivariado, x~exp(alpha+beta*y), y~exp(alpha+beta*x)

x=matrix(0,nrow=10000,ncol=2)
mu=rep(0,10000)
sig=rep(0,10000)

for(k in 1:9999){
  # mu[k+1]=rnorm(1,media,var=sigma^2/n)
  mu[k+1]=rnorm(1,20,sd=sqrt(4/10))
  # sig[k+1]=rgamma(1,n/2,(n(x_barra-mu[k+1])^2+n*sigma^2)/2)^(-1)
  sig[k+1]=rgamma(1,shape=10/2,rate=(10*(20-mu[k+1])^2+10*4)/2)^(-1)
}
