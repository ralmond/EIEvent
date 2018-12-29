\name{ruleType}
\alias{ruleType}
\alias{ruleType,Rule-method}
\alias{priority}
\alias{priority,Rule-method}
\alias{condition}
\alias{condition,Rule-method}
\alias{predicate}
\alias{predicate,Rule-method}
\title{Accessors for Rule objects.}
\description{

  The functions described here access the corresponding fields of an
  \code{\linkS4class{Rule}} object.

}
\usage{
ruleType(x)
\S4method{ruleType}{Rule}(x)
priority(x)
\S4method{priority}{Rule}(x)
condition(x)
\S4method{condition}{Rule}(x)
predicate(x)
\S4method{predicate}{Rule}(x)
}
\arguments{
  \item{x}{An object of class \code{\linkS4class{Rule}} to be accessed.}
}
\value{

  The function \code{ruleType} returns a string which should be one of
  \code{"Status"}, \code{"Observable"}, \code{"Context"},
  \code{"Trigger"}, or \code{"Reset"}.

  The function \code{priority} returns a numeric value with lower values
  indicating higher priority.

  The functions \code{condition} and \code{predicate} return a list
  which represents either the \link{Conditions} or \code{Predicates} of
  the rule.
  
}
\author{Russell Almond}
\references{

  The document \dQuote{Rules Of Evidence} gives extensive documentation
  for the rule system.
  \url{https://pluto.coe.fsu.edu/Proc4/RulesOfEvidence.pdf}. 

  Almond, R. G., Steinberg, L. S., and Mislevy, R.J. (2002).  Enhancing
  the design and delivery of Assessment Systems:  A Four-Process
  Architecture.  \emph{Journal of Technology, Learning, and Assessment},
  \bold{1},
  \url{http://ejournals.bc.edu/ojs/index.php/jtla/article/view/1671}. 

  Almond, R. G., Shute, V. J., Tingir, S. and Rahimi,
  S. (2018). Identifying Observable Outcomes in Game-Based Assessments.
  Talk given at the \emph{2018 Maryland Assessment Research
  Conference}.  Slides:
  \url{https://education.umd.edu/file/11333/download?token=kmOIVIwi},
  Video: \url{https://pluto.coe.fsu.edu/Proc4/Almond-Marc18.mp4}.

  MongoDB, Inc. (2018).  \emph{The MongoDB 4.0 Manual}.
  \url{https://docs.mongodb.com/manual/}.
}
\seealso{

  The \code{\linkS4class{Rule}} object has documentation about
  the type and priority system and \link{Conditions} and
  \link{Predicates} each have detailed descriptions of the
  \code{condition} and \code{predicate} arguments.
  
  Other classes in the EIEvent system:
  \code{\linkS4class{EIEngine}}, \code{\linkS4class{Context}},
  \code{\linkS4class{Status}}, \code{\linkS4class{Event}},
  \code{\linkS4class{RuleTable}}.

  Other methods for working with Rules:
  \code{\link{Rule}}, \code{\link{parseRule}}, 
  \code{\link{verb}}, \code{\link{object}},
  \code{\link{context}}, \code{\link{name}}, \code{\link{doc}}

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

stopifnot(ruleType(r1)=="Observable", priority(r1)==5,
  all.equal(condition(r1),list("event.data.badge"=c("silver","gold"))),
  all.equal(predicate(r1),list("!set"=c("state.observables.badge"=
                                   "event.data.badge"))))


}
\keyword{ manip }
\keyword{ interface }

