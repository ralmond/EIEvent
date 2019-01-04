\name{queryTest}
\alias{queryTest}
\alias{queryTestScript}
\alias{predicateTest}
\alias{predicateTestScript}
\alias{ruleTest}
\alias{ruleTestScript}
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
queryTest(test, quiet = FALSE, verbose = FALSE, name = test$name)
queryTestScript(filename, quiet = FALSE, verbose = FALSE, suiteName = basename(filename))
predicateTest(test, quiet = FALSE, verbose = FALSE, name = test$name)
predicateTestScript(filename, quiet = FALSE, verbose = FALSE, suiteName = basename(filename))
ruleTest(test, quiet = FALSE, verbose = FALSE, contextSet=NULL, name = test$name)
ruleTestScript(filename, quiet = FALSE, verbose = FALSE,
contextSet=NULL, suiteName = basename(filename))
}
\arguments{
  \item{test}{A list with components: \code{name}, \code{doc},
    \code{state}, \code{event}, \code{rule} and \code{result}.  See
    details.} 
  \item{quiet}{A logical value.  If true, progress reports are suppressed.}
  \item{verbose}{A logical value.  If true, more detailed information
    about the test is logged.}
  \item{name}{A name for the test, used for logging.}
  \item{filename}{A pathname or URL giving a JSON file filled with
    condition tests.}
  \item{suiteName}{A name associated with the test scripts for
    reporting.}
  \item{contextSet}{A collection of contexts, used to resovle context
    matching issues.  This should be an object which is suitable as an
    argument to \code{\link{matchContext}}. If \code{contextSet} is
    null, the context matching is not tested.}
}
\details{

  A \code{test} for a rule consists is a list with
  the following commponents:
  \describe{
    \item{name}{An identifier for the test; used in reporting.}
    \item{doc}{Human readable documentation; reported only if
      \code{verbose} is \code{TRUE}.}
    \item{state}{An object of class \code{\linkS4class{Status}} giving
      the state of the system, or a list which can be converted to a
      Status object using \code{\link{parseStatus}}.}
    \item{event}{An object of class \code{\linkS4class{Event}} giving
      the current event, or a list which can be converted to an Event
      object using \code{\link{parseEvent}}.} 
    \item{rule}{The \code{\linkS4class{Rule}} object to be tested, or a
      list which can be converted to a Rule object using
      \code{\link{parseRule}}.   Only the 
      \code{\link{condition}} of the rule is used.}
    \item{result}{For \code{queryTest}, this should be a logical value.
      For \code{predicateTest} or \code{ruleTest} this should be an
      object of class \code{\linkS4class{Status}}, or a list which can
      be coerced to a Status object using \code{\link{parseStatus}}.}
  }


  In the case of \code{queryTestScript}, \code{predicateTestScript}, and
  \code{ruleTestScript} these objects are put into a JSON file and are
  then processed one at a time. 

  The function \code{queryTest} runs \code{\link{checkCondition}} with
  arguments \code{(condition(rule), state, event)} and checks the value
  against the expected result.  The function \code{queryTestScript} runs
  \code{queryTest} on each test in its input stream and returns a vector
  of results. 

  The function \code{predicateTest} runs \code{\link{executePredicate}}
  with arguments \code{(predicate(rule), state, event)} and checks the
  value against the expected result.  The function
  \code{predicateTestScript} runs \code{predicateTest} on each test in
  its input stream and returns a vector of results.

  The function \code{ruleTest} runs the complete check as if it were
  being run through \code{\linkS4class{EIEngine}}.  First it checks to
  see if the \code{\link{verb}} and \code{\link{object}} of the rule and
  event match, and if \code{contextSet} is non-null it checks to make
  sure that the \code{\link{context}} of the rule is applicable for the
  current \code{\link{context}} of the state.  If these checks pass, it
  then checks the condition of the rule.  If the condition is
  statistfied it runs the predicate.  It then checks the value against
  the expected result (which should be the same as the initial state if
  the rule should not fire).  If either the condition or the predicate
  generate an error, the function returns \code{NA}.  The function
  \code{predicateTestScript} runs \code{predicateTest} on each test in
  its input stream and returns a vector of results.

  Unless \code{quiet} is true the functions report on progress and test
  results to standard output.  Setting \code{verbose}  to true adds to
  the amount of output, including adding tracebacks of any errors which
  were encountered.
  

}
\value{

  The functions \code{queryTest}, \code{predicateTest}, and
  \code{ruleTest} return a logical value indicating 
  whether the actual result matched the expected result.  If an error is
  encountered while processing the rule, it is caught and \code{NA} is
  returned from the function.  These function throw an error if its
  \code{state}, \code{event}, \code{rule} and \code{result} components
  are not of the expected type (or cannot be coerced to the correct
  type). 

  The functions \code{queryTestScript}, \code{predicateTestScript}, and
  \code{ruleTestScript} returns a logical vector
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

  The functions \code{queryTest}, \code{predicateTest}, and
  \code{ruleTest} suppress errors on the grounds that
  it is usually better to attempt all of the tests rather than stop at
  the first failure.  This is also true of \code{queryTestScript},
  \code{predicateTestScript}, and \code{ruleTestScript},  which
  also continues after syntax errors in the test file.  Certain errors,
  however, are not caught including errors opening the target file and
  the initial JSON parsing.

}
\seealso{

  \code{\linkS4class{Rule}} describes the rule object, \link{Conditions}
  describes the conditions and \link{Predicates} describes the
  predicates.  The function \code{\link{checkCondition}} tests when
  conditions are satisfied, and \code{\link{executePredicate}} executes
  the predicate.
  
  Other classes in the EIEvent system:
  \code{\linkS4class{EIEngine}}, \code{\linkS4class{Context}},
  \code{\linkS4class{Status}}, \code{\linkS4class{Event}},
  \code{\linkS4class{RuleTable}}.

}
\examples{

## Query Tests
test <- list(
  name="Simple test",
  doc="Demonstrate test mechanism.",
  state = Status("Fred","test"),
  event= Event("Fred","test","rule",details=list(trophy="gold")),
  rule=Rule(condition=list("event.data.trophy"="gold"),ruleType="Status"),
  result=TRUE)

stopifnot(queryTest(test,verbose=TRUE))

test1 <- list(
  name="Simple test",
  doc="Demonstrate test mechanism.",
  state = Status("Fred","test"),
  event= Event("Fred","test","rule",details=list(trophy="silver")),
  rule=Rule(condition=list("event.data.trophy"="gold"),ruleType="Status"),
  result=TRUE)

stopifnot(!queryTest(test1,verbose=TRUE))

stopifnot(all(
queryTestScript(file.path(library(help="EIEvent")$path,"testScripts",
                "CondCheck.json"),verbose=TRUE)
))

## Predicate Tests
testr <- list(
  name="Simple set",
  doc="Demonstrate predicate test mechanism.",
  state = Status("Fred","test",
    timestamp=as.POSIXct("2018-12-21 00:01:01")),
  event= Event("Fred","test","rule",details=list(agent="ramp"),
    timestamp=as.POSIXct("2018-12-21 00:01:01")),
  rule=Rule(predicate=list("!set"=c("state.observables.rampused"=TRUE)),
         ruleType="Observable"),
  result=Status("Fred","test",observables=list("rampused"=TRUE),
    timestamp=as.POSIXct("2018-12-21 00:01:01")))

stopifnot(predicateTest(testr,verbose=TRUE))

testr1 <- list(
  name="Simple test",
  doc="Demonstrate test mechanism.",
  state = Status("Fred","test",
        timestamp=as.POSIXct("2018-12-21 00:01:01")),
  event= Event("Fred","test","rule",details=list(agent="ramp"),
       timestamp=as.POSIXct("2018-12-21 00:01:01")),
  rule=Rule(predicate=list("!set"=c("state.observables.grampused"=TRUE)),
         ruleType="Observable"),
  result=Status("Fred","test",observables=list("rampused"=TRUE),
    timestamp=as.POSIXct("2018-12-21 00:01:01")))

stopifnot(!predicateTest(testr1,verbose=TRUE))

stopifnot(all(
predicateTestScript(file.path(library(help="EIEvent")$path,"testScripts",
                "PredCheck.json"),verbose=TRUE)
))

}
\keyword{ interface }
\keyword{ logic }

