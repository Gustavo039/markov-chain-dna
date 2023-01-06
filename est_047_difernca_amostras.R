##### Variancia posteriori para a razao de 2 variancias de normais segue uma gama Inve.


##data1=um vetor
##data2=um vetor

## S1=var(data1)
## S2=var(data2)

gen_var_post=function(n_gen,data_x){
  y=1/rgamma(n_gen,shape=(nrow(data_x)/2),rate=(nrow(data_x)-1)*var(data_x)/2)
  return(y)
}

library(tidyverse)

data1=mtcars%>%
  select(disp)

data2=mtcars%>%
  select(wt)


hist(gen_var_post(1000,data2)/gen_var_post(1000,data1))


gen_prop_post=function(n_gen,data_x){
  
}


