\name{RuleTest}
\alias{EITest}
\alias{RuleTest}
\alias{parseEITest}
\alias{parseRuleTest}
\alias{as.jlist,EITest,list-method}
\alias{as.jlist,RuleTest,list-method}

\title{Constructor for EITest or Rule Test.}
\description{

  These are the constructors for the \code{\linkS4class{EITest}} and
  \code{\linkS4class{RuleTest}} objects.  As test objects are
  usually read from a database or other input stream, the
  \code{parseEITest} and \code{parseRuleTest} functions recreate test
  objects from a JSON list and \code{\link[mongo]{as.jlist}} encodes
  them into a list to be saved as JSON.

}
\usage{
EITest(name, doc = "", app = "default", initial, event, final)
RuleTest(name = paste("Test of Rule", rule), doc = "",
         app = "default", initial, event, rule, queryResult, final)
parseEITest(rec)
parseRuleTest(rec)
\S4method{as.jlist}{EITest,list}(obj, ml, serialize = TRUE)
\S4method{as.jlist}{RuleTest,list}(obj, ml, serialize = TRUE)
}
\arguments{
  \item{name}{A character string identifying the test.  Used in Logging.}
  \item{doc}{A character string providing a description of the test.}
  \item{app}{A character string identifying the application.}
  \item{initial}{A \code{\linkS4class{Status}} object giving the initial
    state of the system.}
  \item{event}{A \code{\linkS4class{Event}} object giving the triggering
    event. }
  \item{rule}{A \code{\linkS4class{Rule}} object giving rule being tested.}
  \item{queryResult}{A logical value indicating whether or not the
    \code{\link{Conditions}} of the rule are satisfied;  that is,
    whether or not the \code{\link{Predicates}} of the rule should be run.}
  \item{final}{A \code{\linkS4class{Status}} object giving the final
    state of the system.}
  \item{rec}{A named list containing JSON data.}
  \item{obj}{An object of class \code{\linkS4class{RuleTest}} to be
    encoded.}
  \item{ml}{A list of fields of \code{obj}.  Usually, this is created by
    using \code{\link[base]{attributes}(obj)}.}
  \item{serialize}{A logical flag. If true,
    \code{\link[jsonlite]{serializeJSON}} is used to protect the
    \code{data} field (and other objects which might contain complex R
    code).}
}
\details{

  Most of the details about the \code{\linkS4class{EITest}} and
  \code{\linkS4class{RuleTest}}  objects is documented under the class
  page.

  The function \code{as.jlist} converts the \code{obj} into a named
  list. It is usually called from the function
  \code{\link[mongo]{as.json}}.

  The \code{parseEITest} function is the inverse of \code{as.jlist}
  applied to an EITest object, and \code{parseRuleTest} is for a
  RuleTest object. They are designed to be given as an argument
  to \code{\link[mongo]{getOneRec}} and
  \code{\link[mongo]{getManyRecs}}.

}
\value{

  The functions \code{EITest} and \code{parseEITest} return objects
  of class \code{\linkS4class{EITest}}.   The functions
  \code{RuleTest} and \code{parseRuleTest} return objects
  of class \code{\linkS4class{RuleTest}}.  The function \code{as.jlist}
  produces a named list suitable for passing to
  \code{\link[jsonlite]{toJSON}}.

}
\references{

  The document \dQuote{Rules Of Evidence} gives extensive documentation
  for the context system:
  \url{https://pluto.coe.fsu.edu/Proc4/RulesOfEvidence.pdf}.

}
\author{Russell Almond}
\seealso{

  \code{\linkS4class{EITest}} describes the EITest object.
  \code{\linkS4class{RuleTest}} describes the RuleTest object.

  The functions \code{\link{testQuery}}, \code{\link{testPredicate}},
  and \code{\link{testRule}} are used to actually execute the tests.

  \code{\link[Proc4]{buildMessage}} and \code{\link[mongo]{as.json}}
  describe the JSON conversion system.

  The functions \code{\link[mongo]{getOneRec}} and
  \code{\link[mongo]{getManyRecs}} use \code{parseStatus} to extract
  events from a database.

}
\examples{

etest <- EITest(
  name="My First test",
  doc="Demonstrate EI Test mechanism.",
  initial = Status("Fred","test",timestamp=as.POSIXct("2018-12-21 00:01:01")),
  event= Event("Fred","test","rule",details=list(trophy="gold"),
         timestamp=as.POSIXct("2018-12-21 00:01:01")),
  final = Status("Fred","test",
     observables=list("trophy"="gold"),
     timestamp=as.POSIXct("2018-12-21 00:01:01")),
  )

el <- as.jlist(etest,attributes(etest))
etesta <- parseEITest(el)
stopifnot(all.equal(etest,etesta))

test <- RuleTest(
  name="Simple test",
  doc="Demonstrate test mechanism.",
  initial = Status("Fred","test",timestamp=as.POSIXct("2018-12-21 00:01:01")),
  event= Event("Fred","test","rule",details=list(trophy="gold"),
         timestamp=as.POSIXct("2018-12-21 00:01:01")),
  rule=Rule(condition=list("event.data.trophy"="gold"),
            predicate=list("!set"=c("state.observables.trophy"="event.data.trophy")),
            ruleType="Observable"),
  queryResult=TRUE,
  final = Status("Fred","test",
     observables=list("trophy"="gold"),
     timestamp=as.POSIXct("2018-12-21 00:01:01")),
  )

tl <- as.jlist(test,attributes(test))
testa <- parseRuleTest(tl)
stopifnot(all.equal(test,testa))


test1 <- RuleTest(
  name="Simple test",
  doc="Demonstrate test mechanism.",
  initial = Status("Fred","test",timestamp=as.POSIXct("2018-12-21 00:01:01")),
  event= Event("Fred","test","rule",details=list(trophy="silver"),
         timestamp=as.POSIXct("2018-12-21 00:01:01")),
  rule=Rule(condition=list("event.data.trophy"="gold"),
            predicate=list("!set"=c("state.observables.trophy"="event.data.trophy")),
            ruleType="Observable"),
  queryResult=TRUE,
  final = Status("Fred","test",
     observables=list("trophy"="silver"),
     timestamp=as.POSIXct("2018-12-21 00:01:01")),
  )

test1a <- parseRuleTest(as.jlist(test1,attributes(test1)))
stopifnot(all.equal(test1,test1a))

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

testra <- parseRuleTest(as.jlist(testr,attributes(testr)))
stopifnot(all.equal(testr,testra))


testt <- RuleTest(
  name="Simple message",
  doc="Demonstrate trigger test mechanism.",
  initial = Status("Fred","test",
    timestamp=as.POSIXct("2018-12-21 00:01:01"),
    observables=list(badge="silver")),
  event= Event("Fred","level","finished",details=list(badge="silver"),
    timestamp=as.POSIXct("2018-12-21 00:01:01")),
  rule=Rule(predicate=list("!send"=list()),
         ruleType="Trigger"),
  queryResult=TRUE,
  final=P4Message(uid="Fred",mess="Observables Available",
    details=list(badge="silver"),context="test",sender="EIEvent",
    timestamp=as.POSIXct("2018-12-21 00:01:01")))

testtl <- as.jlist(testt,attributes(testt))
testta <- parseRuleTest(testtl)
stopifnot(all.equal(testt,testta))


}
\keyword{ interface }
\keyword{ database }
