\name{queryTest}
\alias{queryTest}
\alias{queryTestScript}
\title{Functions for testing rule queries.}
\description{

  The \code{\linkS4class{Rule}} objects in an
  \code{\linkS4class{EIEngine}} form a program, which requires testing.
  These functions provide a mechanism for testing the query part of the
  rule.  The script gives a \code{\linkS4class{Status}},
  \code{\linkS4class{Event}} and \code{\linkS4class{Rule}} object, and
  then checks to see if the the rule fires or not.  The function
  \code{quertyTest} tests a single rule, and \code{queryTestScript}
  tests a collection of rules found in a JSON file.

}
\usage{
queryTest(test, quiet = FALSE, verbose = FALSE, name = test$name)
queryTestScript(filename, quiet = FALSE, verbose = FALSE, suiteName = basename(filename))
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
}
\details{

  A \code{test} for the condition part of a rule consists is a list with
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
    \item{result}{A logical value inditicated if the condition should be
      met or not.}
  }

  In the case of \code{queryTestScript}, these objects are put into a
  JSON file and are then processed one at a time.

  The function \code{queryTest} runs \code{\link{checkCondition}} with
  arguments \code{(condition(rule), state, event)} and checks the value
  against the expected result.  The function \code{queryTestScript} runs
  \code{queryTest} on each test in its input stream and returns a vector
  of results. 

  Unless \code{quiet} is true the functions report on progress and test
  results to standard output.  Setting \code{verbose}  to true adds to
  the amount of output, including adding tracebacks of any errors which
  were encountered.
  

}
\value{

  The function \code{queryTest} returns a logical value indicating
  whether the result of \code{\link{checkCondition}} matches the
  expected result.  If \code{\link{checkCondition}} throws an error, it
  is caught and reported \code{queryTest} returns \code{NA}.  The
  function \code{queryTest} throws an error if its \code{state},
  \code{event}, \code{rule} and \code{result} components are not of
  the expected type (or cannot be coerced to the correct type).

  The function \code{queryTestScript} returns a logical vector
  indicating the result of each test script.  The values will be true if
  the test passed, false if it failed and code \code{NA} if either
  \code{\link{checkCondition}} or  \code{queryTest} generated an error.
    
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

  The function \code{queryTest} suppresses errors one the grounds that
  it is usually better to attempt all of the tests rather than stop at
  the first failure.  This is also true of \code{queryTestScript} which
  also continues after syntax errors in the test file.  Certain errors,
  however, are not caught including errors opening the target file and
  the initial JSON parsing.

}
\seealso{

  \code{\linkS4class{Rule}} describes the rule object and
  \link{Predicates} describes the predicates.  The function
  \code{\link{checkCondition}} tests when conditions are satisfied.
  
  Other classes in the EIEvent system:
  \code{\linkS4class{EIEngine}}, \code{\linkS4class{Context}},
  \code{\linkS4class{Status}}, \code{\linkS4class{Event}},
  \code{\linkS4class{RuleTable}}.

}
\examples{

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

}
\keyword{ interface }
\keyword{ logic }

