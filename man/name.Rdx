\name{name}
\alias{name}
\title{Returns the name of an object.}
\description{

  This returns a character string identifying the object.

}
\usage{
name(x)
}
\arguments{
  \item{x}{An object whose name is of interest.}
}
\value{

  A character scalar identifying the object.

}
\author{Russell Almond}
\seealso{

  \code{\linkS4class{TableListener}}
}
\examples{

mess1 <- P4Message(app="default",uid="Phred",context="Down Hill",
                   sender="EIEvent",mess="New Observables",
                   details=list(trophy="gold",solvedtime=10))
tabMaker <- TableListener(name="Trophy Table",
                   messSet="New Observables",
                   fieldlist=c(uid="character", context="character",
                               timestamp="character",
                               solvedtime="numeric",
                               trophy="ordered(none,silver,gold)"))

name(tabMaker)

}
\keyword{ manip }

