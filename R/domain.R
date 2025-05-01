domain<-function(d=NULL) 
{
# create new working domain (with standard name .Grpp) and optionally give it an alias
	pos<-1
	env<-as.environment(pos)
	assign('.Grpp',new.env(parent=emptyenv()),env)
	if(!is.null(d)) assign(d,.Grpp,env)
	class(.Grpp)<-c('environment','Grappa')
}

revisit<-function(name=NULL) 
{
# re-set working domain to previously named domain
	pos<-1
	env<-as.environment(pos)
	if(!is.null(name)) assign('.Grpp',get(name,.GlobalEnv),env)
}

list.domains<-function()
{
x<-ls(all.names=TRUE,envir=.GlobalEnv)
x[as.vector(sapply(x,function(z) 'Grappa'%in%class(get(z))))]
}

print.Grappa<-function (x,...) 
{
cat("Grappa domain with",length(x$var.names),"nodes:\n")
cat(x$var.names,fill=60)
cat("  forming",length(x$tcq),"cliques\n")
}


get.nc<-function(d=.Grpp) 
{
# normalising constant
make()
sum(d$tcq[[1]])
}
