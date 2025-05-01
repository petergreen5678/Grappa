# Mix  
# set up DAG

domain('mix')

vs('alleles',c('8','10','11','x'))
.Grpp$gene.freq<-c(.184884,.134884,.233721,.446511)

founder('vmg')
founder('vpg')
genotype('vgt','vmg','vpg')

founder('smg')
founder('spg')
genotype('sgt','smg','spg')

query('T2eqv',,c('V','U'))
query('T1eqs',,c('S','U'))
by('target','T2eqv','T1eqs')

select('T2mg','vmg','T2eqv')
select('T2pg','vpg','T2eqv')

select('T1mg','smg','T1eqs')
select('T1pg','spg','T1eqs')

genotype('T2gt','T2mg','T2pg')
genotype('T1gt','T1mg','T1pg')

mix('mix','T2gt','T1gt')

prop.evid('vgt','8-10')
prop.evid('sgt','8-11')
prop.evid('mix','8-10-11')
pnmarg('target')
