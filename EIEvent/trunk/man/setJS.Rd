\name{setJS}
\alias{setJS}
\alias{setJSfield}
\title{Sets a field in a status object in Javascript notation.}
\description{

  Fields of a \code{\linkS4class{Status}} can be accessed using
  JavaScript notation, e.g., \code{state.flags.}\emph{field},
  \code{state.observables.}\emph{field},or
  \code{state.timers.}\emph{name}. The function \code{getJS} set the
  current value of the referenced field from the state object.

}
\usage{
setJS(field, state, now, value)
setJSfield(target, fieldlist, value)
}
\arguments{
  \item{field}{A character scalar describing the field to be referenced
    (see details).}
  \item{state}{An object of type \code{\linkS4class{Status}} giving the
    current status of the user in the system; this argument will be
    modified. }
  \item{now}{An object of class \code{\link[base]{POSIXt}} which gives
    the time ofthe event.  Used when setting timers.}
  \item{value}{The new value to be assigned to the field.}
  \item{target}{A collection object to be access.  The object implmenting 
    \code{state.flags}, \code{state.observables} or
    \code{event.details}, or one of sub-components.}
  \item{fieldlist}{The successive field names as a vector of characters
    (split at the \sQuote{.} and excluding the initial \code{state.flags},
    \code{state.observables} or \code{event.details}.}
}
\details{

  The \link{Predicates} of \code{\linkS4class{Rule}} objects update 
  parts of the  current \code{state}.  As these rules are typically
  written in JSON, it is natural to reference the parts of the
  \code{\linkS4class{Status}} objects using
  javascript notation.  Javascript, like R, is a weakly typed language,
  and so javascript objects are similar to R lists: a named collection
  of values.  A period, \sQuote{.}, is used to separate the object from
  the field name, similar to the way a \sQuote{$} is used to separate
  the field name from the object reference when working with R lists.
  If the object in a certain field is itself an object, a succession of
  dots.  Thus a typical reference looks like:
  \emph{object.field.subfield} and so forth as needed.

  In EIEvent rules, only the \code{state}, the current
  \code{\linkS4class{Status}}, can be modified.  Therefore, in the
  predicate all dot notation field references start with \code{state}
  (field references starting with \code{event} can be used the 
  \code{event}.  Furthermore, \code{\linkS4class{Status}} 
  objects have only a certain number of settable fields so only those
  fields can be referenced. 

  The state object has two fields which are collections of arbitrary
  object:  \code{state.flags} and \code{state.observables}.  (The state also
  contains a collection of timer objects, \code{state.timers}, which has
  special rules described below.)  Each of these is a named collection
  (list in R), and components can be refenced by name.  The expressions
  \dQuote{\code{state.flags.}\emph{name}}, and
  \dQuote{\code{state.observables.}\emph{name}} reference an object
  named \emph{name} in the flags or
  observables field of the state respectively.  Note that the available
  components of these lists fields will depend on the context of the
  simulation and the verb and object of the event.

  The fields \code{state.flags} and
  \code{state.observables} could also be multipart objects (i.e., R
  lists).  Additional dots can be used to reference the subcomponents.
  Thus \dQuote{\code{state.flags.position.x}} references the
  x-coordinate of the position object in the flag field.  These dots can
  be nested to an aribrary depth.

  The fields of \code{state.flags.} and
  \code{state.observables.} can also contain unnamed vectors (either
  character, numeric, or list).  In this case square brackets can be
  used to index the elements by position.  Indexes start at 1, as in R.
  For example, \dQuote{\code{state.flags.agentList[3]}} references the
  third value in the agentList flag of the status.  Currently only
  numeric indexes are allowed, variable references are not, nor can
  sublists be selected.

  Note that the fields allowed to be set are a subset of the fields in
  which can be accessed (see \code{\link{setJS}} for a complete list).
  In particular, only fields of the \code{state} object can be set,
  while fields of the \code{event} object can also be referenced.  Also,
  certain fields of the \code{state} object are read only.

  The function \code{setJSfield} is an internal function which is used
  to set the components, it is called recursively to modify fields which
  are themselves lists or vectors.
  
}
\section{Fields of the \code{Status} object.}{

  The following fields of the \code{\linkS4class{Status}} object can be
  set:
  
  \itemize{
    \item{\code{state.context} The current context that the state
      object is in.}
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

  The other fields listed in \code{\link{getJS}} can be accessed but not
  set. 
}
\section{Timers}{

  The \code{state.timers} field holds a named list of objects of class
  \code{\linkS4class{Timer}}.  These behave as if they have two settable
  subfields:  \code{running} (or \code{run}) and \code{time} (or
  \code{value}).

  The \code{running} (or \code{run}) virtual field is a logical field:
  \code{TRUE} indicates running and \code{FALSE} indicates paused.
  Setting the value of the field will cause the timer to resume (start)
  or pause depending on the value. 

  The \code{time} (or \code{value}) field gives the elapsed time of the
  timer.  Setting the field to zero will reset the timer to zero,
  setting it to another value will adjust the time.

}

\value{

  The \code{setJS} function always returns the modified state object.
  The \code{setJSfield} function returns the modified colleciton object,
  or if the fieldlist is empty, the new value.

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

  The functions \code{\link{getJS}} for accessing fields and
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
   flags=list("list"=list("one"=1,"two"="too"),"vector"=(1:3)*10,
              "char"="hello"),
   observables=list("list"=list("one"="one","two"=2),"vector"=(1:3),
           "char"="bar"),
   timestamp=as.POSIXct("2018-12-21 00:01"))

ev <- Event("Phred","test","message",
      timestamp=as.POSIXct("2018-12-21 00:01:01"),
      details=list("list"=list("one"=1,"two"=1:2),"vector"=(1:3)))

ts <-timestamp(ev)

st <- setJS("state.context",st,ts,"Level 1")
stopifnot(getJS("state.context",st,ev)=="Level 1",
          getJS("state.oldContext",st,ev)=="Level 0")

st <- setJS("state.observables.numeric",st,ts,12.5)
stopifnot( getJS("state.observables.numeric",st,ev)==12.5)

st <- setJS("state.observables.char",st,ts,"foo")
stopifnot(getJS("state.observables.char",st,ev)=="foo")

st <- setJS("state.observables.list.one",st,ts,"a")
stopifnot(
  all.equal(getJS("state.observables.list",st,ev),list("one"="a","two"=2)),
  getJS("state.observables.list.one",st,ev)=="a")

st <- setJS("state.observables.vector",st,ts,(1:3)*100)
st <- setJS("state.observables.vector[2]",st,ts,20)
stopifnot(
  getJS("state.observables.vector[2]",st,ev)==20,
  all.equal(getJS("state.observables.vector",st,ev),c(100,20,300)))


st <-setJS("state.flags.list.two",st,ts,"two")
stopifnot(all.equal(getJS("state.flags.list",st,ev),list("one"=1,"two"="two")))

st <-setJS("state.flags.vector[3]",st,ts,3)
stopifnot(
  getJS("state.flags.vector[3]",st,ev)==3,
  all.equal(getJS("state.flags.vector",st,ev),c(10,20,3)))

st <- setJS("state.flags.logical",st,ts,TRUE)
stopifnot( getJS("state.flags.logical",st,ev)==TRUE)

st <- setJS("state.flags.char",st,ts,"foobar")
stopifnot(getJS("state.flags.char",st,ev)=="foobar")


}
\keyword{ objects }
\keyword{ manipulation }

