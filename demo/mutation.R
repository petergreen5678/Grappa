# Simple mutation model  
# set up DAG

domain('mutation')

vs('alleles',c('10','12','x'))
.Grpp$gene.freq<-c(0.284,0.259,0.457)

founder('pfmg')
founder('pfpg')
genotype('pfgt','pfmg','pfpg')
query('tfeqpf')
select('tfmg','pfmg','tfeqpf')
select('tfpg','pfpg','tfeqpf')
mendel('copg','tfmg','tfpg')
founder('mmg')
founder('mpg')
genotype('mgt','mmg','mpg')
mendel('comg','mmg','mpg')
select('capg','copg','mutp')
select('camg','comg','mutm')
genotype('cgt','camg','capg')

# loop over values of mutation probability,
# defining rest of model, recompiling, initialising
# and equilibrating, and re-inserting and propagting evidence

pr.mut<-(0:10)/200
for(j in 1:length(pr.mut))
	{
	pr<-pr.mut[j]
	query('mutm',c(1-pr,pr),c('no','yes'))
	query('mutp',c(1-pr,pr),c('no','yes'))

	prop.evid('cgt','10-12',q=T)
	prop.evid('mgt','10-10',q=T)
	prop.evid('pfgt','12-12',q=T)

	cat(pr,nm('tfeqpf'),'\n')
	}
