\name{getJS}
\alias{getJS}
\alias{getJSfield}
\title{Gets a field from an object in Javascript notation.}
\description{

  Fields of a \code{\linkS4class{Status}} can be accessed using
  JavaScript notation, e.g., \code{state.flags.}\emph{field},
  \code{state.observables.}\emph{field},or
  \code{state.timers.}\emph{name}. Similarly, fields of an
  \code{\linkS4class{Event}} can be referenced using \code{event.verb},
  \code{event.object}, \code{event.data.}\emph{field}, or
  \code{event.timestamp}.  The function \code{getJS} fetches the
  current value of the referenced field from the state or event object.

}
\usage{
getJS(field, state, event)
getJSfield(obj,fieldlist)
}
\arguments{
  \item{field}{A character scalar describing the field to be referenced
    (see details).}
  \item{state}{An object of type \code{\linkS4class{Status}} giving the
    current status of the user in the system.}
  \item{event}{An object of type \code{\linkS4class{Event}} giving the
    event being processed.}
  \item{obj}{A collection object to be access.  The object implmenting 
    \code{state.flags}, \code{state.observables} or
    \code{event.details}, or one of sub-components.}
  \item{fieldlist}{The successive field names as a vector of characters
    (split at the \sQuote{.} and excluding the initial \code{state.flags},
    \code{state.observables} or \code{event.details}.}
}
\details{

  Both the \link{Conditions} and \link{Predicates} of
  \code{\linkS4class{Rule}} objects need to reference parts of the
  current \code{state} and \code{event}.  As these rules are typically
  written in JSON, it is natural to reference the parts of the
  \code{\linkS4class{Status}} and \code{\link{Event}} objects using
  javascript notation.  Javascript, like R, is a weakly typed language,
  and so javascript objects are similar to R lists: a named collection
  of values.  A period, \sQuote{.}, is used to separate the object from
  the field name, similar to the way a \sQuote{$} is used to separate
  the field name from the object reference when working with R lists.
  If the object in a certain field is itself an object, a succession of
  dots.  Thus a typical reference looks like:
  \emph{object.field.subfield} and so forth as needed.

  In EIEvent rules, only two objects can be referenced:  \code{state},
  the current \code{\linkS4class{Status}}, and \code{event}, the current
  \code{\linkS4class{Event}}.  Therefore, all dot notation field
  references must start with either \code{state} or \code{event}.
  Furthermore, \code{\linkS4class{Status}} and
  \code{\linkS4class{Event}} objects have only a certain number of
  fields so only those fields can be referenced.   

  The event object has one field which can contain arbitrary
  collections, \code{event.data}.  The state object has two
  \code{state.flags} and \code{state.observables}.  (The state also
  contains a collection of timer objects, \code{state.timers}, which has
  special rules described below.)  Each of these is a named collection
  (list in R), and components can be refenced by name.  The expressions
  \dQuote{\code{event.data.}\emph{name}},
  \dQuote{\code{state.flags.}\emph{name}}, and
  \dQuote{\code{state.observables.}\emph{name}} reference an object
  named \emph{name} in the data field of the event, or the flags or
  observables field of the state respectively.  Note that the available
  components of these lists fields will depend on the context of the
  simulation and the verb and object of the event.

  The fields of \code{event.data}, \code{state.flags} and
  \code{state.observables} could also be multipart objects (i.e., R
  lists).  Additional dots can be used to reference the subcomponents.
  Thus \dQuote{\code{event.data.position.x}} references the
  x-coordinate of the position object in the event data.  These dots can
  be nested to an aribrary depth.

  The fields of \code{event.data}, \code{state.flags.} and
  \code{state.observables.} can also contain unnamed vectors (either
  character, numeric, or list).  In this case square brackets can be
  used to index the elements by position.  Indexes start at 1, as in R.
  For example, \dQuote{\code{state.flags.agentList[3]}} references the
  third value in the agentList flag of the status.  Currently only
  numeric indexes are allowed, variable references are not, nor can
  sublists be selected.

  The function \code{getJSfield} is an internal function which is used
  to access components.  It is called recursively to access fields which
  are themselves lists or vectors.


}
\section{Fields of the \code{Event} object.}{

  The following expressions reference the fields of the
  \code{\linkS4class{Event}} object.
  
  \itemize{
    \item{\code{event.verb} The verb associated with the current event.}
    \item{\code{event.object} The object associated with the current event.}
    \item{\code{event.timestamp} The time at which the event
      occurred.}
    \item{\code{event.data.}\emph{field} The value of the
      extra data field named \emph{field}.}
  }

  The following additional fields can also be referenced, but are seldom
  used.  In many cases, these fields are handled by the
  \code{\linkS4class{EIEngine}} before rule processing begins, so their
  values are irrelevant.
  
  \itemize{
    \item{\code{event.app} The application ID associated with the
    current event.}.
    \item{\code{event.context} The simulator context of the current
    event as recorded by the presentation process.}
    \item{\code{event.uid} The user ID of the student or player.}
    \item{\code{event.message} The message sent from the presentation
    process.  (Often something like \dQuote{New Event}.)}
  }
  
}
\section{Fields of the \code{Status} object.}{

  The following fields of the \code{\linkS4class{Status}} object can be
  referenced. 
  
  \itemize{
    \item{\code{state.context} The current context that the state
      object is in.}
    \item{\code{state.oldContext} The the context of the state at
      the end of the previous event.  In particular, this can be
      compared to the context to check if the context has changed as a
      result of the event.} 
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
  }

  The following additional fields are also recognized, but again they
  are primarily for use in the \code{\linkS4class{EIEngine}}. 

  \itemize{
    \item{\code{state.uid} The user ID of the student or player. (Should
    be the same of that of the event.)}
    \item{\code{state.timestamp} The timestamp of the last event
    encorporated into the status.}
  }
}
\section{Timers}{

  The \code{state.timers} field holds a named list of objects of class
  \code{\linkS4class{Timer}}.  These behave as if they have two settable
  subfields:  \code{running} (or \code{run}) and \code{time} (or
  \code{value}).

  The \code{running} (or \code{run}) virtual field is a logical field:  \code{TRUE}
  indicates running and \code{FALSE} indicates paused.  Setting the
  value of the field will cause the timer to resume (start) or pause
  depending on the value. 

  The \code{time} (or \code{value}) field gives the elapsed time of the
  timer.  Setting the field to zero will reset the timer to zero,
  setting it to another value will adjust the time.

}

\value{

  The value of \code{getJS} the the contents of the referenced field.
  If the referenced field does not exist, or the reference is not well
  formed, then an error is signaled.
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
  
  It is clear that some kind of indirect reference (i.e., using
  variables, either integer or character, inside of the square
  brackects) is needed.  This may be implemented in a future version.
}
\seealso{

  The functions \code{\link{setJS}} for setting fields and
  \code{\link{removeJS}} for removing fields (only allowed with state
  objects). 

  The help files \link{Conditions} and \link{Predicates} each have
  detailed descriptions of rule syntax.
  
  Other classes in the EIEvent system:
  \code{\linkS4class{EIEngine}}, \code{\linkS4class{Context}},
  \code{\linkS4class{Status}}, \code{\linkS4class{Event}},
  \code{\linkS4class{Rule}}.

}
\examples{

st <- Status("Phred","Level 0",timerNames="watch",
   flags=list("list"=list("one"=1,"two"="too"),"vector"=(1:3)*10),
   observables=list("numeric"=12.5,char="foo",
                 "list"=list("one"="a","two"=2),"vector"=(1:3)*100),
   timestamp=as.POSIXct("2018-12-21 00:01"))
context(st) <- "Level 1"

ev <- Event("Phred","test","message",
      timestamp=as.POSIXct("2018-12-21 00:01:01"),
      details=list("list"=list("one"=1,"two"=1:2),"vector"=(1:3)))

stopifnot(
getJS("state.context",st,ev)=="Level 1",
getJS("state.oldContext",st,ev)=="Level 0",
getJS("state.observables.numeric",st,ev)==12.5,
getJS("state.observables.char",st,ev)=="foo",
all.equal(getJS("state.observables.list",st,ev),list("one"="a","two"=2)),
getJS("state.observables.list.one",st,ev)=="a",
getJS("state.observables.vector[2]",st,ev)==200,
all.equal(getJS("state.flags.list",st,ev),list("one"=1,"two"="too")),
getJS("state.flags.list.two",st,ev)=="too",
getJS("state.flags.vector[3]",st,ev)==30
)

stopifnot(
getJS("state.timers.watch.running",st,ev)==FALSE,
getJS("state.timers.watch.time",st,ev)==as.difftime(0,units="secs")
)

timerTime(st,"watch",as.POSIXct("2018-12-21 00:01")) <-
      as.difftime(1,units="mins")
timerRunning(st,"watch", as.POSIXct("2018-12-21 00:01")) <- TRUE

stopifnot(
getJS("state.timers.watch.run",st,ev)==TRUE,
getJS("state.timers.watch.value",st,ev)==as.difftime(61,units="secs")
)
## Note that value of a running timer references difference between
## internal start time and current time as recorded by the event.


stopifnot(
getJS("event.verb",st,ev)=="test",
getJS("event.object",st,ev)=="message",
getJS("event.timestamp",st,ev)==as.POSIXct("2018-12-21 00:01:01"),
all.equal(getJS("event.data.list",st,ev),list("one"=1,"two"=1:2)),
all.equal(getJS("event.data.list.two",st,ev),1:2),
getJS("event.data.vector[3]",st,ev)==3
)

}
