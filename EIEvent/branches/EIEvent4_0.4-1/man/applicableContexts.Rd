\name{applicableContexts}
\alias{applicableContexts}
\alias{belongsTo}
\alias{belongsTo,Context-method}
\alias{belongsTo<-}
\alias{belongsTo<-,Context-method}
\title{Finds context sets to which a given context belongs.}
\description{

  A \code{\linkS4class{Context}} may belong to one or more context sets.
  A \code{\linkS4class{Rule}} may operate on a specific context, a
  context set, or all contexts (the special context set \code{"ALL"}).
  The function \code{applicableContexts} returns a list of all potential
  rule context matches.  The function \code{belongsTo} maintains the
  implicit contexts.

}
\usage{
applicableContexts(c)
belongsTo(c)
\S4method{belongsTo}{Context}(c)
belongsTo(c) <- value
\S4method{belongsTo}{Context}(c) <- value

}
\arguments{
  \item{c}{An object of class \code{\linkS4class{Context}}}
  \item{value}{A character vector containing the names of the context
    sets this context belongs to.}
}
\value{

  The function \code{belongsTo} returns a (possibly empty) vector of
  context set names this context belongs to.  The function
  \code{applicableContexts} returns the same vector with the addition of
  the current context ID and the special context \code{"ALL"}.

}
\references{

  The document \dQuote{Rules Of Evidence} gives extensive documentation
  for the context system:
  \url{https://pluto.coe.fsu.edu/Proc4/RulesOfEvidence.pdf}. 

}
\author{Russell Almond}
\note{

  It would seem a natural extension of this system to put the contexts
  into an acyclic directed graph with the \code{belongsTo} function
  providing the link.  This was determined to be more trouble than it
  was worth for the current application, so the entire hierarchy must be
  represented within the \code{belongsTo} field of each context.

}
\seealso{

  \code{\linkS4class{Context}} describes the context object.

}
\examples{

ct <- Context("Level1","First Tutorial",1,
              belongsTo=c("tutorialLevels","easyLevels"),
              doc="First Introductory Level",
              app="ecd://epls.coe.fsu.edu/EITest")
stopifnot(setequal(belongsTo(ct),c("tutorialLevels","easyLevels")))
stopifnot(setequal(applicableContexts(ct),
          c("Level1","tutorialLevels","easyLevels","ALL")))

belongsTo(ct) <- "tutorialLevels"
stopifnot(setequal(belongsTo(ct),c("tutorialLevels")))

}
\keyword{ manip }

