\name{doc}
\alias{doc}
\alias{doc,Context-method}
\alias{doc,Rule-method}
\alias{name}
\alias{name,Context-method}
\alias{name,Rule-method}
\title{Meta-data accessors for Rules and Contexts.}
\description{

  To provide both \code{\linkS4class{Rule}} and
  \code{\linkS4class{Context}} object with adequate documentation, they
  are given both \code{name} and \code{doc} properties.  The \code{name}
  is used in error reporting and debugging.  The \code{doc} string is
  to aid in rule management.

}
\usage{
doc(x)
\S4method{doc}{Context}(x)
\S4method{doc}{Rule}(x)
name(x)
\S4method{name}{Context}(x)
\S4method{name}{Rule}(x)
}
\arguments{
  \item{x}{A \code{\linkS4class{Context}} or \code{\linkS4class{Rule}}
    object whose documentation is being queried.}
}
\value{

  Both functions return an object of type character.

}
\author{Russell Almond}
\seealso{

  Classes in the EIEvent system:
  \code{\linkS4class{EIEngine}}, \code{\linkS4class{Context}},
  \code{\linkS4class{Status}}, \code{\linkS4class{Event}},
  \code{\linkS4class{Rule}}.

}
\examples{
r1 <- Rule(name="Coin Rule",
           doc="Set the value of the badge to the coin the player earned.",
           app="ecd://coe.fsu.edu/PPtest",
           verb="satisfied", object="game level",
           context="ALL",
           ruleType="Observable", priority=5,
           condition=list("event.data.badge"=c("silver","gold")),
           predicate=list("!set"=c("state.observables.badge"=
                                   "event.data.badge")))

stopifnot(name(r1)=="Coin Rule",grepl("badge",doc(r1)))

ct <- Context("Level1","First Tutorial",1,
              belongsTo=c("tutorialLevels","easyLevels"),
              doc="First Introductory Level",
              app="ecd://epls.coe.fsu.edu/EITest")
stopifnot(name(ct)=="First Tutorial",
          grepl("Intro",doc(ct)))


}
\keyword{ manip }
\keyword{ interface }

