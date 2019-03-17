\name{Predicates}
\alias{Predicates}
\alias{!set}
\alias{!unset}
\alias{!incr}
\alias{!decr}
\alias{!mult}
\alias{!div}
\alias{!min}
\alias{!max}
\alias{!addToSet}
\alias{!pullFromSet}
\alias{!push}
\alias{!pop}
\alias{!start}
\alias{!reset}
\alias{!setCall}
\alias{modify}
\title{Functions that modify state when rule is triggered.}
\description{

  A \code{\linkS4class{Rule}} contains both \link{Conditions} and
  Predicates.  The latter is a list of operations which are run
  when the conditions are satisfied.  The list of predicates has the
  form \emph{!op}\code{=list(}\emph{target}\code{=}\emph{arg}\code{,...)}.
  Here \code{target} is a reference to a field in the
  \code{\linkS4class{Status}} object which is modified by applying the
  \emph{!op} to the current value of the target and the \emph{arg}.

}
\usage{
"!set"(predicate, state, event)
"!unset"(predicate, state, event)
"!incr"(predicate, state, event)
"!decr"(predicate, state, event)
"!mult"(predicate, state, event)
"!div"(predicate, state, event)
"!min"(predicate, state, event)
"!max"(predicate, state, event)
"!addToSet"(predicate, state, event)
"!pullFromSet"(predicate, state, event)
"!push"(predicate, state, event)
"!pop"(predicate, state, event)
"!start"(predicate, state, event)
"!reset"(predicate, state, event)
"!setCall"(predicate, state, event)
modify(predicate, state, event, op)
}
\arguments{
  \item{predicate}{This is a list of the form
    \code{list(}\emph{target1}\code{=}\emph{arg1}\code{, }
    \emph{target2}\code{=}\emph{arg2}\code{, ...)}.
    It names the fields to be modified and the new values (or
    arguments used to compute the new values).}
  \item{state}{A \code{\linkS4class{Status}} object representing the
    current state of the simulation.}
  \item{event}{A \code{\linkS4class{Event}} object giving details of the
    current event.  Used for dereferencing references in the
    \emph{arg}.}
  \item{op}{A binary argument used to combine the value of the
    \emph{target} and the \emph{arg} for the \code{modify} function.}
}
\details{

  The predicate of a \code{\linkS4class{Rule}} is a list of operations.
  Each operation has the following form:

  \emph{!op}\code{=list(}\emph{target}\code{=}\emph{arg}\code{,...)}
    
  Here, \emph{target} is an identifier of a field in the
  \code{\linkS4class{Status}}  object being modified (targets in the
  \code{\linkS4class{Event}} object cannot be modified).  The target field is
  in the dot notation (see \code{\link{setJS}}).  The query operator,
  \emph{!op} is one of the operations described in the section
  \sQuote{Predicate Operators}.  The 
  \emph{arg} is a value or field reference that will be used in
  calculating the new value for the target field.  In other words, the
  statement is effectively of the form \emph{target !op arg}.  The
  \code{...} represents additional 
  \code{target}--\code{arg} pairs to be modified.

  The \emph{arg} can be a literal value (either scalar or vector) or a
  reference to another field in the \code{\linkS4class{Status}} or
  \code{\linkS4class{Event}} object using the dot notation.  

  In general, a predicate contains a list of operators and each operator
  contains a list of \emph{target}\code{=}\emph{arg} pairs.  These are
  each executed sequentially; however, the order is not guarenteed.  If
  order is important, use multiple rules with different priorities.

  Finally, one special operator allows for expansion.  For the
  \code{!setCall} operator, \emph{arg} should be the name of a function,
  with arguments \code{(name, state, event)}, where \code{name} is the
  name of the target field, and state and event are the current state
  and event objects.  The value is set to the value returned.

}
\section{Predicate Operators}{

  The following operators are supported:
  \describe{
    \item{\code{!set}}{This operator sets the field to the value of the
      argument.  If the field is a flag or observable and does not exist, it is
      created. If it is a timer and does not exist, an error is signaled.} 
    \item{\code{!unset}}{This is the inverse of the \code{!set}
      operator.  If the \emph{arg} in this expression is \code{NULL}, or
      \code{NA}, then the field will be set to that value.  If 
      \emph{arg} is \dQuote{Delete}, then the field will be removed.}
    \item{\code{!incr}, \code{!decr}, \code{!mult}, \code{!div},
      \code{!min}, \code{!max}}{For all six of these, the current value
      of the field is replaced by the result of combining it with the
      value of the argument.  The combination funcitons are \sQuote{+},
      \sQuote{-}, \sQuote{*}, \sQuote{/}, \code{min}, and \code{max},
      respecitively.}
    \item{\code{!addToSet}, \code{!pullFromSet}}{These operators assume
      that the value of the field is a vector representing a set.  The
      operator \code{!addToSet} adds the argument to the set (if it is
      not already present), and \code{!pullFromSet} removes the argument
      (if it is  present).}
    \item{\code{!push}}{This assumes that the field is a vector which
      represents a stack.  The new value is pushed onto the front of the
      stack.} 
    \item{\code{!pop}}{This assumes that the field is a vector which
      represents a stack. The first value is removed from the stack.  If
      the argument is a field reference, the field referenced in the
      argument is set to the popped value. If the arugment is numeric,
      then that many values are popped off the stack.}
    \item{\code{!start}, \code{!reset}}{In both cases, the field
      referenced should be a timer.  With no argumets, the \code{!start}
      operator sets the  time value to zero and sets the timer running
      and \code{!reset} sets the timer to zero and does not set it
      running. In both cases, if the timer of the name specified in the
      \emph{field} does not exist, one is created.  If either is given a
      logical argument, then the timer is set to running or not
      according to the argument, overriding the default behavior.  If
      the operator is given a numeric or \code{\link[base]{difftime}}
      argument, then the timer is set to that time. Finally, a argument
      which is a list with both a \code{time} (difftime value) and
      \code{running} (logical value) will put the timer in that state.} 
    \item{\code{\link{!send}}}{Special predicate for trigger rules which
      builds messages to be sent to various listeners.}
    \item{\code{!setCall}}{This is a trap door which allows for
      arbitrary R code to be used to calculate the value of the
      \emph{field}.  The argument should be the name of a function with
      three arguments, the name of the field, the status and the event.
      The field will be set to the value returned by the function.}
  }

}
\section{Setting Timers}{

  Note that timers (see \code{\linkS4class{Timer}}) are treated
  specially.  Each timer has a \code{.run} (or \code{.running}) subfield
  which is true if the timer is running and false if it is paused.  It
  also has a \code{.value} (or \code{.time}) field which represents the
  elapsed time.  
    
  Timers can be set using the \code{!set} operator modifies the state of
  the timer.  Setting the \code{.run} or \code{.running} subfield of the
  timer to a logical value will cause the timer to pause (\code{FALSE})
  or resume (\code{TRUE)}.  Setting setting the the \code{.time} or
  \code{.value} will set the elapsed time.  Similarly, the \code{!incr},
  \code{!decr}, etc. operators can be used to change the time value.

  The \code{!start} and \code{!reset} operations are synonyms for
  \code{!set} with some differences.  First, both a \code{running} (or
  \code{run}) and \code{value} (or \code{time}) can be set at the same
  time.  If only a real or \code{\link[base]{POSIXt}} value is specified
  the it is assumed that the time should be set. If only a logical value
  is supplied, it is assumed that the running state should be set.  If
  the logical value is not supplied, it is assumed to be \code{TRUE} for
  \code{!start} and \code{FALSE} for \code{!reset}.  If the time value
  is not specified, it is assumed to be zero.

  There is one important difference between the \code{!set} and the
  \code{!start} approach.  They behave differently if the timer object
  is not already created in the state object. The \code{!set} operator
  (and related modification operators) will signal an error.  The
  \code{!start} and \code{!reset} operators will create a new timer if
  needed.
  
}
\section{Expansion Mechanisms}{

  The special \dQuote{!setCall} form obviously allows for expansion.  It
  is particularly designed for value calculations which involve multiple
  fields in a complex fashion.

  It is also possible to expand the set of \emph{!op} functions.  The
  method used for dispatch is to call 
  \code{do.call(op,list(predicate, state, event))} where \emph{predicate}
  is a list of \emph{target}\code{=}\emph{arg} pairs.

  The \code{modify} function is a useful tool for building new
  predicates.  It combines the current value of the field with the value
  of the arg using a specified operator.  This is used to implement many
  of the existing operators.

}

\section{Predicate Testing}{

  The function \code{\link{checkCondition}} is used internally to check
  when a set of conditions in a rule are satisfied.

  The functions \code{\link{testPredicate}} and
  \code{\link{testPredicateScript}} 
  can be used to test that predicates function properly.
  The functions \code{\link{testRule}} and \code{\link{testRuleScript}}
  can be used to test that rule conditions and predicates function
  properly together.

  
}

\value{

  An object of class \code{\linkS4class{Status}} with the target
  fields modified.

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

  Don't confuse the \sQuote{!} operator with the character \dQuote{!}
  used at the start of the predicate operator names.

}
\seealso{

  The special \code{\link{!send}} predicate (and
  \code{\link{buildMessages}} is documented separately).

  \code{\linkS4class{Rule}} describes the rule object and
  \link{Conditions} describes the Conditions.

  The functions \code{\link{testRule}} and \code{\link{testRuleScript}}
  can be used to test that rule conditions and predicates function
  properly together.

  
  Other classes in the EIEvent system:
  \code{\linkS4class{EIEngine}}, \code{\linkS4class{Context}},
  \code{\linkS4class{Status}}, \code{\linkS4class{Event}},
  \code{\linkS4class{RuleTable}}.


}

\examples{

list (
## Set a flag and an observable.
"!set"=list("state.flags.agent"="ramp",
            "state.observables.trophy"="gold"),
## Set a timer
"!set"=list("state.timers.learningsupports.time"=as.difftime(0,units="secs"),
            "state.timers.learningsupports.run"=TRUE),
## Delete fields
"!unset"=list("state.flags.agent"="NA", # Set to NA
              "state.flags.slider"="NULL", # Set to NULL
              "state.flags.unneeded"="Delete"), # Delete it. 

## Modify fields.
"!incr" = list("state.observables.objects"=1), # Add one to objects
"!decr" = list("state.flags.helpuse"=1,  # Subtract 1 from help use.
               "state.timers.learningsupport"=as.difftime(1,units="mins")),
               # Subract one minute from the learning support timer.
"!mult" = list("state.flags.value"=2),# Double value.
"!div"  = list("state.flags.value"=2), # Halve value.
"!min"  = list("state.flags.attempts"=5), # Attempts is less than 5
"!max"  = list("state.flags.attempts"=0), # Attempts is at least 0

## Set operators
"!addToSet" =list("state.flags.agents"="lever"), #Add lever to list of agents
"!pullFromSet" =list("state.flags.agents"="lever"),
             #Remove level from agent list
"!push"=list("state.flags.objects"="Object 1"), #Put Object 1 on the stack.
"!pop"=list("state.flags.objects"="state.flags.lastObject"),
       #Pop first object off the stack and set lastObject to its value.

"!setCall"=list("state.flags.value"="myOp"))
       #Set value to return value of myOp.


}
\keyword{ interface }
\keyword{ logic }
