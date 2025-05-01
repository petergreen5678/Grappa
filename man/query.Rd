\name{query}
\alias{query}
\alias{tab}
\alias{vs}
\alias{tables}
\alias{prop.evid}
\alias{equil}
\alias{make}
\alias{compile}
\alias{pnmarg}
\alias{nm}
\alias{fq}
\alias{cs}
\alias{joint}

\title{Creates a binary node}
\description{Creates a binary node.}
\usage{
query(g,probs=c(0.5,0.5),values=c('yes','no'))
}
\arguments{
  \item{g}{name of the node}
  \item{probs}{numerical vector of the probabilities of the two states}
  \item{values}{character string, the values of the two states}
}
\value{NULL, with the side effect of creating a CPT in the current domain.}
\author{Peter J. Green, \email{P.J.Green@bristol.ac.uk}}
\examples{
domain('asia')

query('asia',c(0.01,0.99))
asia$t.asia
}

