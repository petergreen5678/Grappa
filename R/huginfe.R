# exact same or similar to gRaven functions

add.node<-function (domain, name, category = c("chance", 
    "decision", "utility", "function"), kind = c("discrete", "continuous", "other"), 
    subtype, states) 
{
    category <- match.arg(category)
    if(category!="chance") stop("gRaven does not yet handle category =",category)
    kind <- match.arg(kind)
    if(kind!="discrete") stop("gRaven does not yet handle kind =",kind)
    if (missing(states)) {
        if (subtype == "boolean") 
            states <- c(0, 1)
		attr(states,"logical")<-c(FALSE,TRUE)
    }
    else {
        if (missing(subtype)) 
            subtype <- switch(mode(states), character = "labeled", 
                numeric = "numbered", logical = "boolean")
    }
    if(mode(states)=="logical")  
		{
		attr(states,"logical")<-states
		states[] <- c(0, 1)
		}
    if (name %in% domain$nodes) 
        stop(name, " already in domain\n")
    if(any(duplicated(states))) stop("states must be distinct")
    
    domain$nodes <- c(domain$nodes, name)
    domain$states <- c(domain$states, structure(list(states), names = name))
    domain$parents <- c(domain$parents, structure(list(NULL), names = name))
}

add.edge<-function(domain,child,parent)
{
if((!child%in%domain$nodes)||any(!parent%in%domain$nodes)) stop(child,"",parent," not all already in domain\n")
domain$parents[[child]]<-c(domain$parents[[child]],parent)
domain$cptables[[child]]<-NULL
}

# new functions set.table get.marginal

set.table<-function(domain,n,tab)
{
vars<-c(n,get('parents',envir=domain)[[n]])
levels<-sapply(get('states',envir=domain),length)[vars]
probs<-tab
values<-get('states',envir=domain)[[n]]
tab(vars,levels,probs,values)
if(is.null(domain$var.names)||(!n%in%domain$var.names)) domain$var.names<-c(domain$var.names,n)
}

current<-function()
{
for(d in list.domains()) if(d!='.Grpp') 
if(identical(.Grpp,get(d))) break
d
}

quick<-function(domain,v)
{
now<-current()
revisit(domain)
print(get(v,envir=.Grpp))
revisit(now)
}

get.nodes<-function (domain)
{
# model for a function that may need to go to a different domain
get(domain)$var.names
}

get.states<-function (domain,node)
{
# model for a function that may need to go to a different domain
get(cs('vs.',node),envir=get(domain))
}

get.marginal<-function (domain, nodes, class = c("data.frame", "table", "ftable", 
    "numeric")) 
{
# model for a general function that may need to go to a different domain
class <- match.arg(class)
if(is.environment(domain)) domain<-domain$Name
now<-current(); revisit(domain)
make()
fq(nodes)
revisit(now)
}

set.finding<-function (domain, node, finding)
{
if(is.environment(domain)) domain<-domain$Name
now<-current(); revisit(domain)
prop.evid(node,finding)
revisit(now)
}

get.normalization.constant<-function(domain=.Grpp)
{
if(is.character(domain)) domain<-get(domain,envir=.GlobalEnv)
make()
sum(domain$tcq[[1]])
}

