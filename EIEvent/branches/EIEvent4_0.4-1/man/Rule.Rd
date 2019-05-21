\name{Rule}
\alias{Rule}
\alias{parseRule}
\alias{as.jlist,Rule,list-method}
\title{Constructor for EIEvent Rule Objects}
\description{

  The \code{Rule} funciton is the constructor for the
  \code{\linkS4class{Rule}} object.  As Event objects are usually read
  from a database or other input stream, the \code{parseRule} function
  is recreates an event from a JSON list.


}
\usage{
Rule(context = "ALL", verb = "ALL", object = "ALL", ruleType = c("Status", "Observable", "Context", "Trigger", "Reset"), priority = 5, doc = "", name = paste("When in", context, ",", verb, object, ruleType), condition = list(), predicate = list(), app = "default")
parseRule(rec)
\S4method{as.jlist}{Rule,list}(obj, ml, serialize = TRUE)
}
\arguments{
  \item{context}{A character string describing the task, item or game
    level during which the event occurred.  This should be the name of
    an object of class \code{\linkS4class{Context}} and it could
    reference a context set or the special keyword \code{"ALL"}.}
  \item{verb}{A character scalar identifying the action for which the
    rule is appropriate:  could be the special keyword \code{"ALL"}.}
  \item{object}{A character scalar identifying the direct object for
    which the rule is appropriate.  Could be the keyword \code{"ALL"}.}
  \item{ruleType}{A character identifier indicating which phase the rule
    should be run in.  This should be one of the values \code{"State"},
    \code{"Observable"}, \code{"Context"}, \code{"Trigger"}, or
    \code{"Reset"}.  See the \sQuote{Rule Type} section of
    \code{\linkS4class{Event}} for a description of the phases.}
  \item{priority}{A numeric value indicating the order in which the rules
    should be run, lower number running earlier in the sequence.  It is
    recommended to use 5 for typical values and smaller numbers for
    rules which must run earlier, and higher numbers for rules which
    must run later.}
  \item{doc}{A character vector describing the rule in human language.}
  \item{name}{A character scalar giving an identifier fro the rule.
    Primarily used in error reporting, debugging and rule management.}
  \item{condition}{A list in a special \link{Conditions} syntax.  This
    tests the state of the current \code{\linkS4class{Status}} and
    \code{\link{Event}}.  If the test returns true, then the
    \code{predicate} is executed.}
  \item{predicate}{A list in a special \link{Predicates} syntax.  This
    describes the changes that are made to the
    \code{\linkS4class{Status}} object (or other actions) that are taken
    when the rule is triggered.}
  \item{app}{A character scalar providing a unique identifier for the
    application (game or assessment).  This defines the available
    vocabulary for \code{verb} and \code{object}, as well as the set of
    applicable \code{\linkS4class{Rule}} objects.}
  \item{rec}{A named list containing JSON data.}
  \item{obj}{An object of class \code{\linkS4class{Event}} to be
    encoded.}
  \item{ml}{A list of fields of \code{obj}.  Usually, this is created by
    using \code{\link[base]{attributes}(obj)}.}
  \item{serialize}{A logical flag. If true,
    \code{\link[jsonlite]{serializeJSON}} is used to protect the
    \code{data} field (and other objects which might contain complex R
    code.} 
}
\details{

  Most of the details about the \code{\linkS4class{Rule}} object, and
  how it works is documented under \link{Rule-class}.

  The function \code{as.jlist} converts the \code{obj} into a named
  list. It is usually called from the function
  \code{\link[Proc4]{as.json}}.  

  The \code{parseRule} function is the inverse of \code{as.jlist}
  applied to an event object.  It is designed to be given as an argument
  to \code{\link[Proc4]{getOneRec}} and 
  \code{\link[Proc4]{getManyRecs}}.

  Soon to come.  A \code{loadRule} function which will load in rules.
  These operate on JSON files, usually part of a test suite.
  
}
\value{

  The functions \code{Rule} and \code{parseRule} return objects of
  class event.  The function \code{as.jlist} produces a named list
  suitable for passing to \code{\link[jsonlite]{toJSON}}.

}
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
\author{Russell Almond}
\seealso{

  The rule object is described in \code{\linkS4class{Rule}} and 
  \link{Conditions} and \link{Predicates} each have detailed
  descriptions. 
  The functions \code{\link{testQuery}} and \code{\link{testQueryScript}}
  can be used to test that rule conditions function properly.
  The functions \code{\link{testPredicate}} and
  \code{\link{testPredicateScript}} can be used to test that rule
  conditions function properly. 
  The functions \code{\link{testRule}} and \code{\link{testRuleScript}}
  can be used to test that rule conditions and predicates function
  properly together.  The class \code{\linkS4class{RuleTest}} stores a
  rule test. 

  \code{\link[Proc4]{parseMessage}} and \code{\link[Proc4]{as.json}}
  describe the JSON conversion system. 

  The functions \code{\link[Proc4]{getOneRec}} and 
  \code{\link[Proc4]{getManyRecs}} use \code{parseEvent} to extract
  events from a database.

  
  Other classes in the EIEvent system:
  \code{\linkS4class{EIEngine}}, \code{\linkS4class{Context}},
  \code{\linkS4class{Status}}, \code{\linkS4class{Event}},
  \code{\linkS4class{RuleTable}}.


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

r1a <- parseRule(ununboxer(as.jlist(r1,attributes(r1))))
all.equal(r1,r1a)


}
\keyword{ interface }
\keyword{ database }

