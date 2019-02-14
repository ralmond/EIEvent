\name{queryResult}
\alias{queryResult}
\alias{initial}
\alias{final}
\alias{event}
\alias{rule}
\title{Accessor Functions for RuleTest objects.}
\description{

  These are accessor functions for the components of the
  \code{\linkS4class{RuleTest}} class.  These allow extraction but not
  setting of the components.

}
\usage{
queryResult(x)
initial(x)
final(x)
event(x)
rule(x)
}
\arguments{
  \item{x}{A \code{\linkS4class{RuleTest}} object whose fields are to be
    accessed.}
}
\value{

  The function \code{queryTest} returns a logical value indicating
  whether or not the \code{\link{Conditions}} of the rule should return
  true for this test case.

  The function \code{rule} returns the \code{\linkS4class{Rule}} being
  tested.

  The function \code{event} returns the \code{\linkS4class{Event}} being
  processed in the test.

  The function \code{initial} returns the initial
  \code{\linkS4class{Status}} of the system before the test.

  The function \code{final} returns the final
  \code{\linkS4class{Status}} of the system after the test is applied.

}
\author{Russell Almond}
\seealso{

  \code{\linkS4class{RuleTest}} describes the rule test class and
  \code{\link{RuleTest}} the constructor function.


}
\examples{
testr <- RuleTest(
  name="Simple set",
  doc="Demonstrate predicate test mechanism.",
  initial = Status("Fred","test",
    timestamp=as.POSIXct("2018-12-21 00:01:01")),
  event= Event("Fred","test","rule",details=list(agent="ramp"),
    timestamp=as.POSIXct("2018-12-21 00:01:01")),
  rule=Rule(predicate=list("!set"=c("state.observables.rampused"=TRUE)),
         ruleType="Observable"),
  queryResult=TRUE,
  final=Status("Fred","test",observables=list("rampused"=TRUE),
    timestamp=as.POSIXct("2018-12-21 00:01:01")))

name(testr)
doc(testr)
initial(testr)
event(testr)
rule(testr)
queryResult(testr)
final(testr)

}
\keyword{ classes }
