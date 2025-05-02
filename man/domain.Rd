\name{domain}
\alias{domain}
\alias{revisit}
\alias{list.domains}
\alias{print.Grappa}
\alias{print.Grappatab}
\title{Finds a clique, or all cliques, containing a given vertex}
\description{Finds a clique, or all cliques, containing a given vertex.}
\usage{
domain(d)
}
\arguments{
  \item{d}{character string, the desired name of the domain}
}
\details{Creates a new Grappa domain}
\value{a numerical vector of one or more clique indices.}
\author{Peter J. Green, \email{P.J.Green@bristol.ac.uk}}
\examples{
demo('asia')

list.domains()
asia
ls(asia)
}

