\name{runStatusRules}
\alias{runStatusRules}
\alias{runObservableRules}
\alias{runContextRules}
\alias{runTriggerRules}
\alias{runResetRules}
\title{Runs all of the appropriate rules of the given type.}
\description{

  Given a player \code{\linkS4class{Status}} (\code{state}) and an
  \code{\linkS4class{Event}}, this function runs all of the rules
  of the appropriate type and in order of the \code{\link{priority}}.

}
\usage{
runStatusRules(eng, state, event, rules)
runObservableRules(eng, state, event, rules)
runContextRules(eng, state, event, rules)
runTriggerRules(eng, state, event, rules)
runResetRules(eng, state, event, rules)
}
\arguments{
  \item{eng}{An object of class \code{\link{EIEngine}} which is
    responsible for maintaining the rule base and running the rules.}
  \item{state}{An object of class \code{\link{Status}} which gives the
    state of the system before the rules are run.}
  \item{event}{An object of class \code{\link{Event}} which provides
    details of the event being processed.}
  \item{rules}{An list of objects of class \code{\link{Rule}} sorted in
    priority.  The rules of the appropriate type will be selected from
    this list. }
}
\details{

  The functions are run by \code{\link{handleEvent}} in the following
  five phases:
  \enumerate{
    \item{\code{runStatusRules} runs (calls \code{\link{runRule}} on)
      the status \code{\linkS4class{Rule}} objects.  These update the
      internal status (flags and timers) of the \code{state}.  The
      modified state is returned from these rules.}
    \item{\code{runObservableRules} runs the observable
      \code{\linkS4class{Rule}} objects.  These update the external
      status (observables) of the \code{state}.  The modified state
      is returned from these rules.}
    \item{\code{runContextRules} runs the context
      \code{\linkS4class{Rule}} objects.  These update the
      \code{\link{context}} field of the state.  These are run until the
      context changes (is no longer equal to \code{\link{oldContext}})
      or all of the rules have been processed with no change in context.
      The modified state is returned from these rules.}
    \item{\code{runTriggerRules} runs (calls \code{\link{runTRule}} on)
      the trigger \code{\linkS4class{Rule}} objects.  Trigger rules do
      not modify the state, but rather if they are satisfied sends a
      message to the \code{listeners(eng)}.}
    \item{\code{runResetRules} runs (calls \code{\link{runRule}} on) the
      reset \code{\linkS4class{Rule}} objects, but only when the context
      has changed (is different from \code{oldContext}). It resets
      counters, timers and flags.  Note that the logic about whether the
      state has changed is in \code{\link{handleEvent}} not this
      function.}
  }

  The rules are loaded from the database using the \code{findRules}
  method of the \code{\linkS4class{EIEngine}} (which in turn calls the
  \code{findRules} method of the \code{\linkS4class{RuleTable}} it
  contains).  The rule matches the current phase if the following
  conditions hold:
  \itemize{
    \item{The \code{\link{app}} field of the rule matches that of the
      engine (and the state and event).}
    \item{The \code{\link{verb}} and \code{\link{object}} of the rule
      either match those of the event, or have the special value
      \dQuote{ALL}.}
    \item{The \code{\link{context}} of the rule either matches the
      context of the state, or is a context group that contains the
      context of the state (see \code{\link{applicableContexts}}) or is
      the special value \dQuote{ALL}.}
    \item{The \code{\link{ruleType}} of the rule matches the current phase.}
  }
    
  All rules matching these conditions are returned and sorted by the
  rule's \code{\link{priority}}.  Ties are handled arbitrarily.  The
  rules are then run (using \code{\link{runRule}}) in the returned
  order.  If an error occurs in running any of the rules, then further
  processing will stop and an object of class \code{try-error} will be
  returned. 

  Two exceptions:  First, the context rules are only run until the
  context changes.  As soon as the context changes, the function
  \code{runContextRules} exits and returns the modified state.
  Second, the function \code{runTriggerRules} uses \code{runTRule}
  instead of \code{runRule}.  Instead of modifiying the state, it sends
  messages to the engine's listeners.

  Rules are processed in the \code{\link[Proc4]{withFlogging}}
  environment, so depending on the current threshold, various
  information will be provides about which rules are run.  Also, if an
  error occurs, all of the functions will return an object of class
  \code{try-error} instead of their normal return value.  This can be
  used to suspend processing. 

}
\value{

  If processing was successful the possibly modified \code{state} (an
  object of class \code{\linkS4class{Status}} will be returned.

  If processing was unsuccessful, then an object of class
  \code{try-error} will be returned.

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

  The signature of this function has changed for reasons of efficiency.
  In previous versions, each function did a very simlilar database
  check.  In practice, a lot of time was spent checking the database to
  find that there were no applicable rules.  Now, the function
  \code{\link{processEvent}} finds the applicable rules, and the
  functions merely select the ones that match the status.  If no rules
  are available, a considerable amount of time is saved.

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
processEvent <- function (eng,state,event) {
  flog.debug("")                        #Blank line for visibility.
  flog.info("New Event for %s: (%s %s) %s",uid(event),
             verb(event),object(event),
             toString(timestamp(event)))
  rules <- eng$findRules(verb(event),object(event),context(state))
  if (length(rules)==0L) {
    flog.info("No rules for event, skipping.")
    return (NULL)
  }
  out <- runStatusRules(eng,state,event)
  if (is(out,'try-error')) return (out)
  else state <- out
  out <- runObservableRules(eng,state,event)
  if (is(out,'try-error')) return (out)
  else state <- out
  out <- runContextRules(eng,state,event)
  if (is(out,'try-error')) return (out)
  else state <- out
  runTriggerRules(eng,state,event)
  if (oldContext(state) != context(state)) {
    out <- runResetRules(eng,state,event)
    if (is(out,'try-error')) return (out)
    else state <- out
    state@oldContext <- context(state)
  }
  state@timestamp <- timestamp(event)
  state
}

}
}
\keyword{ manip }

