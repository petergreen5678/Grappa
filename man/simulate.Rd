\name{simulate.dir}
\alias{simulate}
\title{Simulates values from the current model}
\description{Simulates values from the current model.}
\usage{
simulate(nobs=1,start=1)
}
\arguments{
  \item{nobs}{numeric scalar, number of observations}
  \item{start}{numeric scalar, index of initial node for graph search}
}
\value{numerical matrix with \code{nobs} rows, indices of the simulated values}
\author{Peter J. Green, \email{P.J.Green@bristol.ac.uk}}
\examples{
domain('asia')

query('asia',c(0.01,0.99))
query('smoke')
tab(c('tb','asia'),,c(.05,.95,.01,.99),c('yes','no'))
tab(c('cancer','smoke'),,c(.1,.9,.01,.99),c('yes','no'))
tab(c('bronc','smoke'),,c(.6,.4,.3,.7),c('yes','no'))
or('tbcanc','tb','cancer')
tab(c('xray','tbcanc'),,c(.98,.02,.05,.95),c('yes','no'))
tab(c('dysp','tbcanc','bronc'),,c(.9,.1,.8,.2,.7,.3,.1,.9),c('yes','no'))

simulate(5)
}

