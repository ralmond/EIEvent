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

  The function \code{processEvent} runs the following functions in
  sequence:
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
  It then updates the timestamp.

  The function \code{handleEvent} first finds the current status for the
  user from the \code{\linkS4class{userRecordSet}} linked to the
  engine. At the end, it saves the updated status.
}
\value{

  The both function return the updated state
  (\code{\linkS4class{Status}}) object, although \code{handleEvent} also
  saves it to the \code{\linkS4class{UserRecordSet}}.  If an error
  occurs, the error is logged and an object of type \code{try-error} is
  returned instead.
  
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

## See test/EngineTest.R

}
\keyword{ manip }

