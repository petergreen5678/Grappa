# Simple illustration of Bayes theorem - blood tests  
# set up DAG

domain('bloodtests')

tab('disease',,c(0.95,0.05),c('harmless','serious'))
tab('number',4,rep(0.25,4))
p<-c(.04,.98)
P<-array(0,c(5,2,4))
for(i in 1:2) for(n in 1:4) P[,i,n]<-dbinom(0:4,n,p[i])
tab(c('test','disease','number'),c(5,2,4),P,0:4)

prop.evid('number',1)
prop.evid('test',1,usevs=T)
pnmarg('disease')

equil()
prop.evid('number',3)
prop.evid('test',1,usevs=T)
pnmarg('disease')
