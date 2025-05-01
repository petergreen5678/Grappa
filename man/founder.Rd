\name{founder}
\alias{founder}
\alias{genotype}
\alias{mendel}
\alias{mix}
\alias{or}
\alias{and}
\alias{select}
\alias{by}
\alias{which}

\title{Creates a founder gene node}
\description{Creates a founder gene node.}
\usage{
founder(g, freq) 
}
\arguments{
  \item{g}{character string, desired name of the node}
  \item{freq}{probabilities. If missing, uses \code{gene.freq} in the current domain}
}
\value{NULL}
\author{Peter J. Green, \email{P.J.Green@bristol.ac.uk}}
\examples{
demo('mix')

founder('abc')

mix$t.abc
}

