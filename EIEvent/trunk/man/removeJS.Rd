\name{removeJS}
\alias{removeJS}
\alias{removeJSfield}
\title{Removes a field from a state object.}
\description{

  Fields of a \code{\linkS4class{Status}} can be accessed using
  JavaScript notation, e.g., \code{state.flags.}\emph{field},
  \code{state.observables.}\emph{field},or
  \code{state.timers.}\emph{name}. The function \code{removeJS} sets
  removes the field.  This function called when the \code{\link{!unset}}
  operator is called with the \code{"Delete"} argument.

}
\usage{
removeJS(field, state)
removeJSfield(target, fieldlist)
}
\arguments{
  \item{field}{A character scalar describing the field to be removed
    (see details).}
  \item{state}{An object of type \code{\linkS4class{Status}} giving the
    current status of the user in the system; this argument will be
    modified. }
  \item{target}{A collection object to be accessed.  The object implmenting 
    \code{state.flags}, \code{state.observables} or
    \code{state.timers}, or one of sub-components.}
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
  Furthermore, only elements of the collection fields of the
  \code{\linkS4class{Status}}  (\code{flags}, \code{observables} and
  \code{timers}) can be referenced.  The expressions
  \dQuote{\code{state.flags.}\emph{name}}, and
  \dQuote{\code{state.observables.}\emph{name}} reference an object
  named \emph{name} in the flags or
  observables field of the state respectively.

  The fields \code{state.flags} and
  \code{state.observables} could also be multipart objects (i.e., R
  lists).  Additional dots can be used to reference the subcomponents.
  Thus \dQuote{\code{state.flags.position.x}} references the
  x-coordinate of the position object in the flag field.  These dots can
  be nested to an aribrary depth.    The function \code{removeJSfield}
  is a helper function used to remove components of nested items.

  The \code{removeJS} method is called by \code{\link{executePredicate}}
  when the argument to \code{\link{!unset}} is not \code{NULL} or
  \code{NA}. 

}
\value{

  The \code{setJS} function always returns the modified state object.
  The \code{setJSfield} function returns the modified colleciton object,
  or if the fieldlist is empty, \code{NULL}.

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
  \code{\link{setJS}} for setting fields (only allowed with state
  objects).  This function is called from
  \code{\link{executePredicate}}. 

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

st1 <- removeJS("state.flags.char",st)
stopifnot(all("char"!=names(st1@flags)))

st1 <- removeJS("state.flags.list.two",st)
stopifnot(length(flag(st1,"list"))==1L)

st1 <- removeJS("state.observables.char",st)
stopifnot(is.null(obs(st1,"char")))

st1 <- removeJS("state.observables.list.one",st)
stopifnot(length(obs(st1,"list"))==1L)

st1 <- removeJS("state.timers.watch",st)
stopifnot(is.null(timer(st1,"watch")))

}
\keyword{ objects }
\keyword{ manipulation }

