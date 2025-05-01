# An ordinary Markov chain

domain('chain')

tab(0,5,rep(0.2,5))
for(i in 1:20) tab(c(i,i-1),c(5,5),c(
 .7,.3,0,0,0,
 .3,.4,.3,0,0,
 0,.3,.4,.3,0,
 0,0,.3,.4,.3,
 0,0,0,.3,.7))
prop.evid(10,2)
fq(0:20,tr=T)
