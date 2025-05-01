# Mixtures with an unknown number of contributors

domain('unknown')

vs('alleles',c('A','B','C'))
.Grpp$gene.freq<-c(.566,.429,.005)
nmax<-2

for(id in c('v','s',paste('u',1:nmax,sep='')))
      {founder(cs(id,'mg'));  founder(cs(id,'pg'))}
query('v.in.mix',,c('V','U'))
query('s.in.mix',,c('S','U'))
tab('n.unknown',nmax+1,rep(1/(nmax+1),nmax+1),nmax:0)
by('target','v.in.mix','s.in.mix','n.unknown')
for(al in .Grpp$vs.alleles)
      {
      for(id in c('v','s',paste('u',1:nmax,sep='')))
            or(cs(id,'has',al),cs(id,'mg'),cs(id,'pg'),
                  al==.Grpp$vs.alleles,al==.Grpp$vs.alleles)
      for(id in c('s','v'))
            and(cs(al,id),cs(id,'has',al),cs(id,'.in.mix'))
      for(n in 1:nmax)
            {
            id<-cs('u',n)
            and(cs(al,id),cs(id,'has',al),'n.unknown',,(nmax:0)>=n)
            }
      or(cs(al,'m',0),cs(al,'v'),cs(al,'s'))
      for(n in 1:nmax)
            or(cs(al,'m',n),cs(al,'m',n-1),cs(al,'u',n))
      }

prop.evid(cs('Am',nmax),'yes')
prop.evid(cs('Bm',nmax),'yes')
prop.evid(cs('Cm',nmax),'no')

prop.evid('vhasA','yes')
prop.evid('vhasB','yes')

prop.evid('shasA','yes')
prop.evid('shasB','no')
prop.evid('shasC','no')

pnmarg('target')
print(joint(c('v.in.mix','s.in.mix')))

fq('n.unknown',tr=T,va=T)
