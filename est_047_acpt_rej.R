gen_values.beta=function(alpha,beta){
  
  ##Armazenando a função alva
  func_alva=function(x) dbeta(x,shape1=alpha,shape2 = beta)
  
  ##Armazenando a função candidata
  func_candidata=function(x)dunif(x,min=0,max=1)
  
  alva=vector()
  rejeitado=vector()
  M <- optimize(f = function(x)func_alva(x)/func_candidata(x), interval = c(0,1), maximum = T)$objective
  
  #Utilizando o metodo da aceitação rejeicao para a criacao dos valores aleatorios
  j=1
  k=1
  for(i in 1:1000){
    u=runif(1)
    y=runif(1)
    if(u*M<=func_alva(y)/func_candidata(y)){
      alva[j]=  y
      j=j+1
    }
    else{
      rejeitado[k]=y
      k=k+1
  }
  }  
  
  
  
  proporcao=(j/1000)*100
  hist(alva,freq=F,col='red')
  curve(func_alva, add = T,col='black',lwd=2)
  
  list_ret=list(Acept=alva,Reject=rejeitado)
  return(list_ret)
}

gen_values.norm=(function(mean_x,sd_x){
  
  ##Armazenando a função alva
  func_alva=function(x) dnorm(x,mean_x,sd_x)
  
  ##Armazenando a função candidata
  func_candidata=function(x)dt(x,)
  
  alva=vector()
  rejeitado=vector()
  M <- optimize(f = function(x)func_alva(x)/func_candidata(x), interval = c(0,1), maximum = T)$objective
  
  #Utilizando o metodo da aceitação rejeicao para a criacao dos valores aleatorios
  j=1
  k=1
  for(i in 1:1000){
    u=runif(1)
    y=runif(1)
    if(u*M<=func_alva(y)/func_candidata(y)){
      alva[j]=  y
      j=j+1
    }
    else{
      rejeitado[k]=y
      k=k+1
    }
  }  
  
  
  
  proporcao=(j/1000)*100
  hist(alva,freq=F,col='red')
  curve(func_alva, add = T,col='black',lwd=2)
  
  list_ret=list(Acept=alva,Reject=rejeitado)
  return(list_ret)
}
)


data1=gen_values.beta(2,5)
data2=gen_values.norm(45,1)

plot(data1$Acept1,1:618)


