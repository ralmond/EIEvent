\name{Status-class}
\Rdversion{1.1}
\docType{class}
\alias{Status-class}
\alias{as.jlist,Status,list-method}
\alias{context,Status-method}
\alias{copy,Status-method}
\alias{flag,Status-method}
\alias{flag<-,Status-method}
\alias{obs,Status-method}
\alias{obs<-,Status-method}
\alias{setTimer,Status-method}
\alias{timer,Status-method}
\alias{timer<-,Status-method}
\alias{timerRunning,Status-method}
\alias{timerRunning<-,Status-method}
\alias{timerTime,Status-method}
\alias{timerTime<-,Status-method}

\title{Class \code{"Status"}}
\description{

  A \code{Status} object represents the state of a student (user) in the
  simulation.  In particular, it provides lists of \code{flags},
  \code{timers} and \code{observables} whose values are updated by the
  \code{\linkS4class{Rule}} objects after an
  \code{\linkS4class{Event}}.  

}
\section{Objects from the Class}{

Objects can be created by calls of the function \code{\link{Status}(...)}.
Generally, the \code{\linkS4class{EIEngine}} maintains one status object
for every student in the system.

}
\section{Slots}{
  \describe{
    \item{\code{app}:}{Object of class \code{"character"} giving an
      identifier for the application. }
    \item{\code{uid}:}{Object of class \code{"character"} giving an
      identifier for the user or student.}
    \item{\code{context}:}{Object of class \code{"character"} giving 
      an identifier for the scoring context the student is currently in.}
    \item{\code{oldContext}:}{Object of class \code{"character"} giving
      an identifier for the previous scoring context.  In particluar, if
      this value is not equal to \code{context} it means the context has
      recently chagned.}
    \item{\code{timers}:}{A named \code{"list"} of \code{\linkS4class{Timer}}
      objects representing events that need to be timed. }
    \item{\code{flags}:}{A named \code{"list"} of fields representing
      the state of the system.  Unlike observables, flags are generally
      not reported outside the system. }
    \item{\code{observables}:}{A named \code{"list"} of fields
      representing details of the task interaction which will be
      reported outside of the evidence identification system.}
    \item{\code{timestamp}:}{Object of class \code{"POSIXt"} giving the
      timestamp of the last \code{\linkS4class{Event}} processed for
      this student. }
  }
}
\section{Methods}{
  \describe{
    \item{as.jlist}{\code{signature(obj = "Status", ml = "list")}:
      preprocessing method used to conver the \code{Status} into a JSON
      string.  See \code{\link[Proc4]{as.jlist}}. }
    \item{context}{\code{signature(x = "Status")}: returns the value of
      the \code{context} field. }
    \item{copy}{\code{signature(prototype = "Status")}: creates a
      duplicate of the status object. }
    \item{flag}{\code{signature(x = "Status")}: returns the list of
      flags associated with the status. }
    \item{flag<-}{\code{signature(x = "Status")}: sets the list of flags
      associated with the status.}
    \item{obs}{\code{signature(x = "Status")}: returns the list of
      observables associated with the status. }
    \item{obs<-}{\code{signature(x = "Status")}: sets the list of
      observables associated with the status. }
    \item{setTimer}{\code{signature(x = "Status", name = "character")}:
      sets the state of the named timer. }
    \item{timer}{\code{signature(x = "Status", name = "character")}:
      returns the named timer object. }
    \item{timer<-}{\code{signature(x = "Status", name = "character")}:
      sets the named timer object. }
    \item{timerRunning}{\code{signature(x = "Status", name =
        "character")}: returns the running status of the named timer.} 
    \item{timerRunning<-}{\code{signature(x = "Status", name =
        "character")}: sets the running status of the named timer. }
    \item{timerTime}{\code{signature(x = "Status", name = "character")}:
      returns the elapsed time of the named timer.}
    \item{timerTime<-}{\code{signature(x = "Status", name = "character")}: 
      sets the elapsed time of the named timer.}
  }
  Note that the \code{\link{getJS}} and \code{\link{setJS}} functions
  are often used to get and set the values of the status object.

}
\section{Header Fields}{

  A \code{Status} object always has header fields.  The \code{app} field
  references the application (assessment) that this state belongs to.
  The \code{uid} field is the student (user or player) the status
  represents.  The \code{timestamp} field is set to the timestamp of the
  last event processed (after it is processed).

  The \code{context} and \code{oldContext} fields operate as a pair.
  Before the event is processed, \code{oldContext} is set to the current
  value of \code{context}.  If the value of \code{context} changes (in
  particular, as result of a context rule, see
  \code{\linkS4class{Rule}}), then this can be determined by comparing
  the value of \code{context} and \code{oldContext}.

}
\section{Flags and Observables}{

  The \code{flags} and \code{observables} fields are both named
  collections of arbitrary R objects.  The exact values stored here will
  depend on the logic of the application.  In general, these can be
  scalar numeric, character or logical variables, vectors of the same,
  or more complex objects made from named lists.  It is probably not
  good to use formal S3 or S4 objects as these won't be properly saved
  and restored from a database.

  The difference between the \code{flags} and \code{observables} is
  a convention that is not enforced in the code.  The \code{observables}
  are intended to be reported using the trigger rules (see
  \code{\linkS4class{Rule}}).  The \code{flags} are
  meant to hold intermediate values that are used to calculate
  \code{observables}.  Nominally, status rules are used to update flags
  and observable rules to update observables, but this is not enforced.

}
\section{Timers}{

  The \code{timers} field holds a named list of objects of class
  \code{\linkS4class{Timer}}.  These behave as if they have two settable
  subfields:  \code{running} (or \code{run}) and \code{time} (or
  \code{value}).

  The \code{running} virtual field is a logical field:  \code{TRUE}
  indicates running and \code{FALSE} indicates paused.  Setting the
  value of the field will cause the timer to resume (start) or pause
  depending on the value.

  The \code{time} field gives the elapsed time of the timer.  Setting
  the field to zero will reset the timer to zero, setting it to another
  value will adjust the time.

}
\section{Dot (Javascript) Field Reference}{

  Fields in the state object can be referenced using a pseudo-Javascript
  dot notation, where nested components are referenced through the
  \sQuote{.} operator (which operates similarly to the R \sQuote{$}
  operator).  These all start with \code{state.} to distringuish them
  from fields in the event.  In particular, the follwing fields are
  recognized. 

  \itemize{
    \item{\code{state.context} The current context that the state
      object is in.}
    \item{\code{state.oldContext} The the context of the state at
      the end of the previous event.  In particular, this can be
      compared to the context to check if the context has changed as a
      result of the event.  (This field should not be set by user code).} 
    \item{\code{state.observables.}\emph{field} The value of the
      observable named \emph{field}.}
    \item{\code{state.timers.}\emph{field} The the timer named
      \emph{field}.  Note that
      \code{state.timers.}\emph{field}\code{.time} or \code{.value} refers
      to the current elapsed time of the timer, and
      \code{state.timers.}\emph{field}\code{.run} or \code{.running} is
      a logical value which refers to whether or not the timer is running.}
    \item{\code{state.flags.}\emph{field} The value of the
      observable named \emph{field}.}
    \item{\code{state.timestamp} The time at which the last processed event
      occurred (this field is read-only).}

  The functions \code{\link{getJS}} and \code{\link{setJS}} are used to
  access the fields, and the help for those functions contains a number
  of examples.

}
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

  Other classes in the EIEvent system:
  \code{\linkS4class{EIEngine}}, \code{\linkS4class{Context}},
  \code{\linkS4class{Rule}}, \code{\linkS4class{Event}},
  \code{\linkS4class{RuleTable}}, \code{\linkS4class{Timer}}.
  
  The functions \code{\link{setJS}}, \code{\link{getJS}} and
  \code{\link{removeJS}} have details
  about using the dot notation to reference fields in the status.  Rule 
  \link{Conditions} and \link{Predicates} also reference fields in the
  status object.

  Methods for working with States:
  \code{\link{timer}}, \code{\link{timerTime}},
  \code{\link{timerRunning}}, \code{\link{flag}}, \code{\link{objs}},
  \code{\link{copy}}, \code{\link{parseStatus}}, 

}
\examples{
showClass("Status")
}
\keyword{classes
}