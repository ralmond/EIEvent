\name{executePredicate}
\alias{executePredicate}
\title{Executes the predicate of an EIEvent Rule.}
\description{

  An \code{\linkS4class{Rule}} object contains a list of 
  \link{Predicates}.  The name of each condition is the name of an
  operator and its value is a list giving the field of
  the \code{\linkS4class{Status}} object to be modified as a name and a
  new value as the value.  For example, 
  \code{"!incr"=c("state.flags.agentCount"=1)} would increment the
  agent count flag by one.  The value can also be a reference to fields
  in the \code{\linkS4class{Status}} or \code{\linkS4class{Event}}
  object.  For example, \code{"!set"=c("state.observables.trophy" =
  "event.data.trophy")} would set the value of the trophy observable to
  the value of the trophy datum in the event.

}
\usage{
executePredicate(predicate, state, event)
}
\arguments{
  \item{predicate}{A named list of actions:  see details.}
  \item{state}{An object of class \code{\linkS4class{Status}} to be
    modified }
  \item{event}{An object of class \code{\linkS4class{Event}} which will
    be referenced in setting the values. }
}
\details{

  The predicate of a \code{\linkS4class{Rule}} is a list of actions to
  be taken when the rule is satisfied.  
  Each action has the following form:

  \emph{!op}\code{=list(}\emph{field}\code{=}\emph{arg}\code{,...)}

  Here, \emph{field} is an identifier of a field in the
  \code{\linkS4class{Status}} object being modified (the
  \code{\linkS4class{Event}} fields cannot be modified).  This is in the
  dot notation (see \code{\link{setJS}}).  The setting operator,
  \emph{!op} is one of the operators described in \link{Predicates}.
  For the \code{"!set"} operator, the \emph{arg} is the replacement
  value or field reference that the field will be used as the
  replacement value.  Most of the other operators use \emph{arg} to
  modify the value of \emph{field} and then replace the value of
  \emph{field} with the result.  For example, the \code{"!incr"}
  operator acts much like the C \code{+=} operator.
  The \code{...} represents additional \code{field}--\code{arg} pairs to
  be set.
  
  The \emph{arg} can be a literal value (either scalar or vector) or a
  reference to another field in the \code{\linkS4class{Status}} or
  \code{\linkS4class{Event}} object using the dot notation.  Note that
  certain operations on timers use the \code{\link[Proc4]{timestamp}}
  field of the Event to update the timer.

  In general, a predicate contains a list of actions.  These are
  executed sequentially, but no guarentees are made about the order.

  Finally, one special operator allows for expansion.  For the
  \code{"!setCall"} operator, \emph{arg} should be the name of a function,
  with arguments \code{(name, state, event)}, where \code{name} is the
  name of the target field, and state and event are the current state
  and event objects.  The value is set to the value returned.

  See \link{Predicates} for more details.
  
}
\value{

  The function \code{executePredicate} returns the modified status object.

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

  \code{\linkS4class{Rule}} describes the rule object and
  \link{Predicates} describes the conditions.  \link{Conditions}
  describes the condition part of the rule, and
  \code{\link{checkCondition}} checks the conditions.

  The functions \code{\link{testPredicate}} and
  \code{\link{testPredicateScript}} 
  can be used to test that rule conditions function properly.
  
  Other classes in the EIEvent system:
  \code{\linkS4class{EIEngine}}, \code{\linkS4class{Context}},
  \code{\linkS4class{Status}}, \code{\linkS4class{Event}},
  \code{\linkS4class{RuleTable}}.


}
\examples{

st <- Status("Phred","Level 1",timerNames=c("learningsupports"),
   flags=list(lastagent="lever",agentlist=c("lever"),
              noobj=7,noagents=0),
   observables=list(),
   timestamp=as.POSIXct("2018-12-21 00:01"))

ev <- Event("Phred","test","message",
      timestamp=as.POSIXct("2018-12-21 00:01:01"),
      details=list(agent="lever"))


## Set a flag and an observable.
st1 <- executePredicate(list("!set"=list("state.flags.agent"="ramp",
            "state.observables.trophy"="gold")),st,ev)
stopifnot(getJS("state.flags.agent",st,ev)=="ramp",
          getJS("state.observables.trophy",st,ev)=="gold")

## Set a timer
st1 <- executePredicate(list("!set"=
                             list("state.timers.learningsupports.time"=
                                   as.difftime(0,units="secs"),
                                  "state.timers.learningsupports.run"=TRUE)),
                        st,ev)
stopifnot(
   timerRunning(st1,"learningsupports",as.POSIXct("2018-12-21 00:01:01")),
   timerTime(st1,"learningsupports",as.POSIXct("2018-12-21 00:01:11"))==
   as.difftime(10,units="secs"))


## Delete fields
st1 <- executePredicate(
  list("!unset"=list("state.flags.noobj"="NA", # Set to NA
                     "state.flags.lastagent"="NULL", # Set to NULL
                     "state.flags.noagents"="Delete")), # Delete it. 
   st,ev)
stopifnot(is.na(flag(st1,"noobj")),is.null(flag(st1,"lastagent")),
          is.null(flag(st1,"noagents")))


## Modify fields.
st1 <- executePredicate(
   list("!incr" = list("state.flags.noagents"=1,
                       "state.timers.learningsupports"=
                       as.difftime(1,units="mins"))),
   st,ev)
stopifnot(flag(st1,"noagents")==1,
   timerTime(st1,"learningsupports",as.POSIXct("2018-12-21 00:01:11"))==
   as.difftime(60,units="secs"))

st2 <- executePredicate(
   list("!decr" = list("state.flags.noagents"=1,
                       "state.timers.learningsupports"=
                       as.difftime(30,units="secs"))),
   st1,ev)
stopifnot(flag(st2,"noagents")==0,
   timerTime(st2,"learningsupports",as.POSIXct("2018-12-21 00:01:11"))==
   as.difftime(30,units="secs"))


st1 <- executePredicate(
   list("!mult" = list("state.flags.noobj"=2)),st,ev)
stopifnot(flag(st1,"noobj")==14)
st2 <- executePredicate(
   list("!div"  = list("state.flags.noobj"=2)),st1,ev)
stopifnot(flag(st2,"noobj")==7)

st1 <- executePredicate(
   list("!min"  = list("state.flags.noobj"=5)),st,ev)
stopifnot(flag(st1,"noobj")==5)
st1 <- executePredicate(
   list("!max"  = list("state.flags.noagents"=1)),st,ev)
stopifnot(flag(st1,"noagents")==1)

## Set operators
st1 <- executePredicate(
   list("!addToSet" =list("state.flags.agentlist"="lever")),st,ev)
stopifnot(flag(st1,"agentlist")=="lever")
st2 <- executePredicate(
   list("!addToSet" =list("state.flags.agentlist"="springboard")),st1,ev)
stopifnot(setequal(flag(st2,"agentlist"),c("lever","springboard")))

st3 <- executePredicate(
   list("!pullFromSet" =list("state.flags.agentlist"="lever")),st2,ev)
stopifnot(flag(st3,"agentlist")=="springboard")


st1 <- executePredicate(
   list("!push"=list("state.flags.objects"="Object 1")), st,ev)
stopifnot(flag(st1,"objects")=="Object 1")

st2 <- executePredicate(
   list("!pop"=list("state.flags.objects"="state.flags.lastObject")),st1,ev)
       #Pop first object off the stack and set lastObject to its value.
stopifnot(flag(st2,"lastObject")=="Object 1",
          length(flag(st2,"objects"))==0L)

myOp <- function(name,state,event) {
  return(getJS("state.flags.noobj",state,event)/
         as.double(getJS("state.timers.learningsupports.time",state,event),
         units="mins"))
}

st1 <- executePredicate(
   list("!setCall"=list("state.flags.value"="myOp")),st,ev)
       #Set value to return value of myOp.
stopifnot(!is.finite(flag(st1,"value")))

}
\keyword{ interface }
\keyword{ logic }

