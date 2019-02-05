\name{testRule}
\alias{testQuery}
\alias{testQueryScript}
\alias{testPredicate}
\alias{testPredicateScript}
\alias{testRule}
\alias{testRuleScript}
\title{Functions for testing rule queries.}
\description{

  The \code{\linkS4class{Rule}} objects in an
  \code{\linkS4class{EIEngine}} form a program, which requires testing.
  These functions provide a mechanism for testing the rules.  The script
  gives a \code{\linkS4class{Status}}, 
  \code{\linkS4class{Event}} and \code{\linkS4class{Rule}} object, and
  then checks to see if the the rule achieves the expected result or
  not.  The functions \code{queryTest}, \code{predicateTest}, and
  \code{ruleTest} test a single rule, and the functions
  \code{queryTestScript}, \code{predicateTestScript}, and
  \code{ruleTestScript} test a collection of rules found in a JSON
  file. 

}
\usage{
testQuery(test)
testQueryScript(filename, suiteName = basename(filename))
testPredicate(test)
testPredicateScript(filename, suiteName = basename(filename))
testRule(test, contextSet=NULL)
testRuleScript(filename, suiteName = basename(filename), contextSet=NULL)
}
\arguments{
  \item{test}{An object of class \code{\linkS4class{RuleTest}}.
    See details.} 
  \item{filename}{A pathname or URL giving a JSON file filled with
    rule tests.}
  \item{suiteName}{A name associated with the test scripts for
    reporting.}
  \item{contextSet}{A collection of contexts, used to resovle context
    matching issues.  This should be an object which is suitable as an
    argument to \code{\link{matchContext}}, either a \code{list} or an
    object of class \code{\linkS4class{ContextSet}}.  If
    \code{contextSet} is null, the context matching is not tested.}
}
\details{

  A \code{test} is a \code{\linkS4class{RuleTest}} class which has the
  following components:
  \describe{
    \item{name}{An identifier for the test; used in reporting.}
    \item{doc}{Human readable documentation; reported only if
      \code{verbose} is \code{TRUE}.}
    \item{inital}{An object of class \code{\linkS4class{Status}} giving
      the initial state of the system.}
    \item{event}{An object of class \code{\linkS4class{Event}} giving
      the current event.}
    \item{rule}{The \code{\linkS4class{Rule}} object to be tested.}
    \item{queryResult}{A logical value indicating whether or not the
      \code{\link{Conditions}} of the rule are satisfied.  If this value
      is false, then \code{testPredicate} will skip the test.}
    \item{final}{This should be an object of class
      \code{\linkS4class{Status}}.  If \code{queryResult} is true, this
      should be the final state of the system after the predicate is
      run.  If \code{queryResult} is false, this should be the same as
      the initial state.}
  }

  The function \code{testQuery} runs \code{\link{checkCondition}} with
  arguments \code{(condition(rule), initial, event)} and checks the value
  against \code{queryResult}.

  The function \code{predicateTest} runs \code{\link{executePredicate}}
  with arguments \code{(predicate(rule), initial, event)} and checks the
  value against \code{final}.  If \code{queryResult} is false, the test
  does not run \code{executePredicate} and always returns true.

  The function \code{ruleTest} does the complete testing of the rules.
  It checks to make sure that the \code{\link{verb}} and
  \code{\link{object}} of the rule and event match, and if
  \code{contextSet} is not null, it checks the context as well.  It then
  runs first \code{\link{checkCondition}} and then
  \code{\link{executePredicate}} if the condition returns true.  The
  result is checked against \code{final}.

  The functions \code{testQueryScript}, \code{testPredicateScript} and
  \code{testRuleScript} run a suite of tests stored in a JSON file and
  return a logical vector of results, with an \code{NA} if an error was
  generated in the condition check or predicate execution.

}
\section{Logging}{

  The results are logged using \code{\link[futile.logger]{flog.logger}}
  to a logger named \dQuote{EIEvent} (that is the package name).  This
  can be redirected to a file or used to control the level of detail in
  the logging.  In particular,
  \code{flog.appender(appender.file("/var/log/Proc4/EIEvent_log.json"),
  name="EIEvent")} would log status messages to the named file.
  Furthermore, \code{flog.layout(layout.json,name="EIEvent")} will cause
  the log to be in JSON format; useful as the inputs are logged as JSON
  objects which facilitates later debugging.

  The amount of information can be controlled by using the
  \code{\link[futile.logger]{flog.threshold}} command.  In particular,
  \code{flog.threshold(level,name="EIEvent")}.  The following
  information is provided at each of these levels (this is cumulative):

  \describe{
    \item{FATAL}{Fatal errors are logged.}
    \item{ERROR}{All errors are logged.}
    \item{WARN}{Warnings are logged.}
    \item{INFO}{Tests are logged as they are run, as are results.  Final
      suite results are logged.}
    \item{DEBUG}{Test doc strings are printed. Additional context
      information (rule, initial, event and final) are printed on test
      failure.  On errors, additional context information (rule, intial
      and event) are provided along with a stack trace.  }
    \item{TRACE}{No additional information is included at this level.}
  }
  
}
\value{

  The functions \code{testQuery}, \code{testPredicate}, and
  \code{testRule} return a logical value indicating 
  whether the actual result matched the expected result.  If an error is
  encountered while processing the rule, it is caught and logged and
  \code{NA} is returned from the function.  

  The functions \code{testQueryScript}, \code{testPredicateScript}, and
  \code{testRuleScript} returns a logical vector
  indicating the result of each test.  The values will be true if
  the test passed, false if it failed and code \code{NA} if either
  an error occured in either parsing or executing the test.
    
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
\note{

  The functions \code{testQuery}, \code{testPredicate}, and
  \code{testRule} suppress errors (using
  \code{\link[Proc4]{withFlogging}}) on the grounds that
  it is usually better to attempt all of the tests rather than stop at
  the first failure.  This is also true of \code{testQueryScript},
  \code{testPredicateScript}, and \code{testRuleScript},  which
  also continue after syntax errors in the test file.  Certain errors,
  however, are not caught including errors opening the target file and
  the initial JSON parsing.

}
\seealso{

  \code{\linkS4class{Rule}} describes the rule object, \link{Conditions}
  describes the conditions and \link{Predicates} describes the
  predicates.  The function \code{\link{checkCondition}} tests when
  conditions are satisfied, and \code{\link{executePredicate}} executes
  the predicate.  \code{\linkS4class{RuleTest}} describes the test
  object. 
  
  Other classes in the EIEvent system:
  \code{\linkS4class{EIEngine}}, \code{\linkS4class{Context}},
  \code{\linkS4class{Status}}, \code{\linkS4class{Event}},
  \code{\linkS4class{RuleTable}}.

}
\examples{

## Query Tests
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
## This test should succeed.
stopifnot(testQuery(test))

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

## This test should fail query check, query needs to allow gold or
## silver trophies.
stopifnot(!testQuery(test1))


stopifnot(all(
testQueryScript(file.path(library(help="EIEvent")$path,"testScripts",
                "CondCheck.json"))
))
 
## Predicate Tests
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

stopifnot(testPredicate(testr))

testr1 <- RuleTest(
  name="Simple test",
  doc="Demonstrate test mechanism.",
  initial = Status("Fred","test",
        timestamp=as.POSIXct("2018-12-21 00:01:01")),
  event= Event("Fred","test","rule",details=list(agent="ramp"),
       timestamp=as.POSIXct("2018-12-21 00:01:01")),
  rule=Rule(predicate=list("!set"=c("state.observables.grampused"=TRUE)),
         ruleType="Observable"),
  queryResult=TRUE,
  final=Status("Fred","test",observables=list("rampused"=TRUE),
    timestamp=as.POSIXct("2018-12-21 00:01:01")))

stopifnot(!testPredicate(testr1))

stopifnot(all(
testPredicateScript(file.path(library(help="EIEvent")$path,"testScripts",
                "PredCheck.json"))
))


nocoin <- Status(uid="Test0", context="Level 84",
                 timestamp=as.POSIXlt("2018-09-25 12:12:28 EDT"),
                 observables=list(badge="none"))
scoin <- Status(uid="Test0", context="Level 84",
                 timestamp=as.POSIXlt("2018-09-25 12:12:28 EDT"),
                 observables=list(badge="silver"))
gcoin <- Status(uid="Test0", context="Level 84",
                 timestamp=as.POSIXlt("2018-09-25 12:12:28 EDT"),
                 observables=list(badge="gold"))

nevent <- Event(app="https://epls.coe.fsu.edu/PPTest",
            uid="Test0",verb="satisfied",
            object="game level", context="Level 84",
            timestamp=as.POSIXlt("2018-09-25 12:12:28 EDT"),   
            details=list(badge="none"))
sevent <- Event(app="https://epls.coe.fsu.edu/PPTest",
            uid="Test0",verb="satisfied",
            object="game level", context="Level 84",
            timestamp=as.POSIXlt("2018-09-25 12:12:28 EDT"),   
            details=list(badge="silver"))
gevent <- Event(app="https://epls.coe.fsu.edu/PPTest",
            uid="Test0",verb="satisfied",
            object="game level", context="Level 84",
            timestamp=as.POSIXlt("2018-09-25 12:12:28 EDT"),   
            details=list(badge="gold"))

crule <- Rule(name= "Coin Rule",
            doc= "Set the value of the badge to the coin the player earned.",
            verb= "satisfied", object= "game level",
            context= "ALL", ruleType= "observables",
            priority= 2, condition= list(event.data.badge=c("silver","gold")),
            predicate= list("!set"=c(state.observables.badge =
                                     "event.data.badge")))

stopifnot(testRule(RuleTest(name="Gold coin test",initial=nocoin,event=gevent,
                   rule=crule,queryResult=TRUE,final=gcoin)))
stopifnot(testRule(RuleTest(name="Silver coin test",initial=nocoin,event=sevent,
                   rule=crule,queryResult=TRUE,final=scoin)))
stopifnot(testRule(RuleTest(name="No coin test",initial=nocoin,event=nevent,
                   rule=crule,queryResult=FALSE,final=nocoin)))



stopifnot(all(
testRuleScript(file.path(library(help="EIEvent")$path,"testScripts",
                "Coincheck.json"))
))




}
\keyword{ interface }
\keyword{ logic }

