\name{runRule}
\alias{runRule}
\alias{runTRule}
\title{Runs a specific rule in a particular application.}
\description{

  The function \code{runRule} runs a \code{\linkS4class{Rule}} against a
  particular \code{\linkS4class{Status}} (state) and
  \code{\linkS4class{Event}}.  The function \code{runTRule} is a special
  version for trigger rules that sends the generated message, if any, to
  the listeners.

}
\usage{
runRule(state, event, rule, phase)
runTRule(state, event, rule, listeners)
}
\arguments{
  \item{state}{An object of class \code{\linkS4class{Status}} which
    describes the current state for the person.}
  \item{event}{An object of class \code{\linkS4class{Event}} which
    describes the current event being processed.}
  \item{rule}{An object of class \code{\linkS4class{Rule}} which is the
    rule to be executed.}
  \item{phase}{An object of type character giving the phase being
    executed, used mainly for logging and error reporting.}
  \item{listenerSet}{An object of class \code{\linkS4class{Listener}}
    (often a \code{\linkS4class{ListenerSet}} which will receive the
    messages from the trigger rules.}
}
\details{

  The function \code{runRule()} runs the rule logic.  For more about
  rule logic, see \code{\linkS4class{Rule}}.

  The function first runs \code{\link{checkCondition}} to check if the
  rule is statisfied or not. If the condition is satisfied, then the
  predicate of the rule will be run using
  \code{\link{executePredicate}}.  The resulting state object 
  is returned.

  The function \code{runTRule()} is similar, but meant specifically for
  trigger rules. Instead of \code{executePredicate}, the function
  \code{\link{buildMessages}} is used to build a list of messages.
  These are then sent to the \code{listenerSet} argument using its
  \code{notifyListeners} method.

  The rules are executed under \code{\link[Proc4]{withFlogging}}
  protection.  This means that if an error is encountered, the error
  message is logged (along with debugging information depending on the
  current logging threshold).  In this case, and object of class
  \code{try-error} is returned instead of the normal error return.

}
\value{

  The function \code{runRule} returns the modified
  \code{\linkS4class{Status}} object.  The function \code{runTRule}
  returns a list of \code{\linkS4class{P4Message}} objects.  In either
  case, if an error occurs, then an object of class \code{try-error} is
  returned instead of the normal value.
  
}
\references{
  The document \dQuote{Rules Of Evidence} gives extensive documentation
  for the rule system.
  \url{https://pluto.coe.fsu.edu/Proc4/RulesOfEvidence.pdf}. 
}
\author{Russell Almond}
\note{

  This function uses the \code{\link[futile.logger]{flog.logger}}
  mechanism.  The following information is reported at various
  thresholds:
  \describe{
    \item{TRACE}{The results of \code{checkCondition} are reported.}
    \item{DEBUG}{When an error occurs information about the state, event
      and rule where the error occurred as well a stack trace are
      logged.}
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

### runRule

st0 <- Status(uid="Test0",context="Stairs",
             timestamp=as.POSIXct("2018-09-25 12:12:28 EDT"),
             observables=list(agentsUsed=list(),
                              lastAgent=NA))
evnt1 <- Event(uid="Test0",
               verb="identified",object="game object",
               context="Stairs",
               timestamp=as.POSIXct("2018-09-25 12:12:29 EDT"),
               details= list("gemeObjectType"="Lever"))
st1exp <- Status(uid="Test0",context="Stairs",
             timestamp=as.POSIXct("2018-09-25 12:12:28 EDT"),
             observables=list(agentsUsed=list("Lever"),
                              lastAgent="Lever"))
r2o <- Rule(name="Update agent used.",
            doc="Adds the agent to the agent list, and sets the last agent.",
            context="Sketching", verb="identified",
            object="game object", ruleType="Observable",
            condition=list(event.data.gemeObjectType=c("Ramp", "Lever",
             "Springboard", "Pendulum")),
            predicate=list("!set"=list("state.observables.lastAgent"=
                                       "event.data.gemeObjectType"),
                           "!push"=list("state.observables.agentsUsed"=
                                        "event.data.gemeObjectType")))
        
st1act <- runRule(st0,evnt1,r2o,"Observable")
stopifnot(all.equal(st1exp,st1act))

### runTRule
st2 <- Status(uid="Test0",context="Stairs",
             timestamp=as.POSIXct("2018-09-25 12:13:30 EDT"),
             observables=list(agentsUsed=list("Lever"),
                              lastAgent="Lever",
                              badge="silver"))
evnt2 <- Event(uid="Test0",
               verb="satisfied",object="game level",
               context="Stairs",
               timestamp=as.POSIXct("2018-09-25 12:13:30 EDT"),
               details= list("badge"="silver"))
r4t <- Rule(name="Satisfied Trigger",
            doc="When the level is satisifed, send the observables.",
            context="ALL", verb="satisfied",
            object="game level", ruleType="Trigger",
            condition=list(),
            predicate=list("!send"=list(mess="Observables Available",
                                        context="state.oldContext",
                                        data=list())))

mess1 <- buildMessages(predicate(r4t),st2,evnt2)[[1]]

cl <- new("CaptureListener")


runTRule(st2,evnt2,r4t,cl)
mess1a <- cl$lastMessage()
stopifnot(all.equal(mess1a,mess1,check_ids=FALSE))


}
\keyword{ manip }
