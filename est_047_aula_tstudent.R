n1=30
xb=131.37
s1_2=26.31

n2=30
yb=136.17
s2_2=28.63

m1=rt(1000,n1-1)*sqrt(s1_2/n1)+xb
m2=rt(1000,n2-1)*sqrt(s2_2/n1)+yb


hist(m1)
hist(m2)

d=m2-m1
hist(d)
shapiro.test(m1)
###############################
#teste para proporção

prop_a=195/396
prop_b=198/460


#Posterioris individuais 
post_a=rbeta(1000,shape1=1.5+195,shape2=(1.5+396-195))
post_b=rbeta(1000,shape1=1.5+198,shape2=(1.5+460-198))

hist(post_a)
hist(post_b)

d=post_b-post_a

hist(d)

k=sum(d<0)/1000

################################33

#Dado 2 amostras, queremos calcular o I.C para a razaõ das variancias a posteriori

n1=30
xb=131.37
s1_2=26.31

n2=30
yb=136.17
s2_2=28.63

##Posteriori para as variancias
post_a=1/rgamma(10000,shape =(n1-1)/2,rate=(n1-1)*s1_2/2 )
post_b=1/rgamma(10000,shape =(n2-1)/2,rate=(n2-1)*s2_2/2 )

d=hist(post_a/post_b)
