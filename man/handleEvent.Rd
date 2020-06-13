\name{handleEvent}
\alias{processEvent}
\alias{handleEvent}
\title{Does the processing for a new event.}
\description{

  This function finds the appropriate rules for handling an event then
  runs them.  It is the core function for the
  \code{\linkS4class{EIEngine}}. 

}
\usage{
handleEvent(eng, event)
processEvent(eng, state, event)
}
\arguments{
  \item{eng}{An object of class \code{\linkS4class{EIEngine}} which will
    process the event.}
  \item{state}{An object of class \code{\linkS4class{Status}} that will
    be updated by the rules processing the event.}
  \item{event}{An object of class \code{\linkS4class{Event}} that will
    be processed.}
}
\details{

  The function \code{processEvent} first calls \code{eng$findRules} to
  find rules appropriate for the event.  If no rules exist, it returns
  NULL (indicating that the state has not changed and does not need to
  be saved.).  If there are rules, then the following functions are run
  in sequence:
  \enumerate{
    \item{\code{\link{runStatusRules}}:  update internal status
      variables (flags and timers).}
    \item{\code{\link{runObservableRules}}:  update external status
      variables (observables).}
    \item{\code{\link{runContextRules}}:  check to see if the context
      (game level) has changed.}
    \item{\code{\link{runTriggerRules}}:  run rules to general messages
      for other processes if necessary.}
    \item{\code{\link{runResetRules}}:  resets the state for the
      beginning of a new context.}
  }
  It then updates the timestamp and returns the modified status.  If an
  error occurs during any of the steps, then an object of class
  \code{try-error} is returned (see \code{\link{withFlogging}}).

  The function \code{handleEvent} first finds the current status for the
  user from the \code{\linkS4class{UserRecordSet}} linked to the
  engine. It then runs \code{processEvent}, and then if necessary it
  saves the updated status. 
}
\value{

  The function \code{processEvent} returns \code{NULL} if there were no
  applicable rules, an object of class \code{\linkS4class{Status}} if
  the rules executed correctly and an object of class \code{try-error}
  if processing the rules generated an error.
  
  The function \code{handleEvent} returns the current
  \code{\linkS4class{Status}} if the handling was successful and an
  object of class \code{try-error} in an error was generated.
  
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

  This function uses the \code{\link[futile.logger]{flog.logger}}
  mechanism.  The following information is reported at various
  thresholds:
  \describe{
    \item{TRACE}{
      \itemize{
        \item{The results of \code{checkCondition} are reported.}
        \item{The specific rules found from each query are reported.}
        \item{A message is logged as each rule is run.}
    }}
    \item{DEBUG}{
      \itemize{
        \item{When an error occurs information about the state, event
          and rule where the error occurred as well a stack trace are
          logged.}
        \item{Each event is logged as it is processed.}
        \item{Rule searches are logged and the number of rules
          reported.}
        \item{A message is logged when each phase starts.}
        \item{A message is logged when the context changes.}
    }}
    \item{INFO and above}{If an error occurs the error is logged along
      with context information.}
  }
}
\seealso{
  \link{Conditions} and \link{Predicates} each have detailed
  descriptions. The functions \code{\link{checkCondition}} and
  \code{\link{executePredicate}} run the condition and predicate parts
  of the rule.  The functions \code{\link{runRule}} and
  \code{\link{runTRule}} run the indivual rules, and the functions
  \code{\link{runTriggerRules}}, \code{\link{runStatusRules}},
  \code{\link{runObservableRules}}, \code{\link{runResetRules}},
  and \code{\link{runContextRules}} run sets of rules.

  The functions \code{\link{testQuery}} and \code{\link{testQueryScript}}
  can be used to test that rule conditions function properly.
  The functions \code{\link{testPredicate}} and
  \code{\link{testPredicateScript}} 
  can be used to test that predicates function properly.
  The functions \code{\link{testRule}} and \code{\link{testRuleScript}}
  can be used to test that rule conditions and predicates function
  properly together.  The class \code{\linkS4class{RuleTest}} stores a
  rule test. 
  
  Other classes in the EIEvent system:
  \code{\linkS4class{EIEngine}}, \code{\linkS4class{Context}},
  \code{\linkS4class{Status}}, \code{\linkS4class{Event}},
  \code{\linkS4class{RuleTable}}.
}

\examples{

\dontrun{
app <- "ecd://epls.coe.fsu.edu/P4test"
loglevel <- "DEBUG"
cleanFirst <- TRUE
eventFile <- "/home/ralmond/Projects/EvidenceID/c081c3.core.json"
## Adjust the path here as necessary
source("/usr/local/share/Proc4/EIini.R")
flog.appender(appender.file(logfile))
flog.threshold(loglevel)

## Setup Listeners
listeners <- lapply(names(EI.listenerSpecs),
                    function (ll) do.call(ll,EI.listenerSpecs[[ll]]))
names(listeners) <- names(EI.listenerSpecs)
if (interactive()) {
  cl <- new("CaptureListener")
  listeners <- c(listeners,cl=cl)
}
EIeng.params <-
  c(EI.config$EIEngine,
    EIeng.local[setdiff(names(EIeng.local),names(EI.config$EIEngine))])

  EIeng.params$listenerSet <-
    ListenerSet(sender= sub("<app>",sapp,EI.config$sender),
                dbname=EIeng.local$dbname, dburi=EIeng.local$dburi,
                listeners=listeners,
                colname=EI.config$lscolname)
  EIeng.params$app <- app

## Make the EIEngine
eng <- do.call(EIEngine,EIeng.params)

## Process Event file if supplied
if (!is.null(eventFile)) {
  system2("mongoimport",
          sprintf('-d %s -c Events --jsonArray', eng$dbname),
          stdin=eventFile)
  ## Count the number of unprocessed events
  NN <- eng$eventdb()$count(buildJQuery(app=app(eng),processed=FALSE))
}

for (n in 1L:NN) {
  eve <- eng$fetchNextEvent()
  out <- handleEvent(eng,eve)
  eng$setProcessed(eve)
}
}
}
\keyword{ manip }

