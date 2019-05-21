\name{asif.difftime}
\alias{asif.difftime}
\alias{is.difftime}
\title{More flexible constructor for creating difftime objects.}
\description{

  The function \code{asif.difftime} is a constructor for
  \code{\link[base]{difftime}} objects from a list with components named
  \dQuote{secs}, \dQuote{mins},   \dQuote{hours}, \dQuote{days}, or
  \dQuote{weeks}.  These get added together.

  The function \code{is.difftime} is the test function missing from the
  base package.

}
\usage{
asif.difftime(e2)
is.difftime(x)
}
\arguments{
  \item{e2}{This should be a list of numeric values with named
    components with names selected from: \code{c("secs", "mins", 
    "hours", "days", "weeks")}.}
  \item{x}{An object to be tested for its difftime status.}
}
\value{

  If the argument to \code{asif.difftime} is a list with the appropriate
  names, then an object of class difftime is returned.  Otherwise, the
  argument is returned.

  The function \code{is.difftime} returns a logical value indicating
  whether or not its argument is of class \code{\link[base]{difftime}}

}
\author{Russell Almond}
\seealso{

  \code{\link[base]{difftime}}4

  Also, \code{asif.difftime} is used in variuos predicates.
  
}
\examples{

dt <- asif.difftime(list(mins=1,secs=5))
stopifnot (is.difftime(dt),
    all.equal(dt,as.difftime(65,units="secs")))


}
\keyword{ chron }

