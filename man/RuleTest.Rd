\name{RuleTest}
\alias{RuleTest}
\alias{parseRuleTest}
\alias{as.jlist,RuleTest,list-method}

\title{Constructor for Rule Test.}
\description{

  This is the constructor for the \code{\linkS4class{RuleTest}} objects
  and context set objects (which are identical).  As RuleTest objects are
  usually read from a database or other input stream, the
  \code{parseRuleTest} function is recreates an event from a JSON list
  and \code{\link[Proc4]{as.jlist}} encodes them into a list to be saved
  as JSON.

}
\usage{
RuleTest(name = paste("Test of Rule", rule), doc = "", app = "default", initial, event, rule, queryResult, final)
parseRuleTest(rec)
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

  Most of the details about the \code{\linkS4class{RuleTest}} object, and
  how it works is documented under \link{RuleTest-class}.    

  The function \code{as.jlist} converts the \code{obj} into a named
  list. It is usually called from the function
  \code{\link[Proc4]{as.json}}.  

  The \code{parseRuleTest} function is the inverse of \code{as.jlist}
  applied to a context object.  It is designed to be given as an argument
  to \code{\link[Proc4]{getOneRec}} and 
  \code{\link[Proc4]{getManyRecs}}.   

}
\value{

  The functions \code{RuleTest} and \code{parseRuleTest} return objects
  of class \code{\linkS4class{Context}}.  The function \code{as.jlist}
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

  \code{\linkS4class{RuleTest}} describes the RuleTest object.

  The functions \code{\link{testQuery}}, \code{\link{testPredicate}},
  and \code{\link{testRule}} are used to actually execute the tests.

  \code{\link[Proc4]{parseMessage}} and \code{\link[Proc4]{as.json}}
  describe the JSON conversion system.  

  The functions \code{\link[Proc4]{getOneRec}} and 
  \code{\link[Proc4]{getManyRecs}} use \code{parseStatus} to extract
  events from a database.
  
}
\examples{

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

}
\keyword{ interface }
\keyword{ database }
