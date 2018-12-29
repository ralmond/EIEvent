\name{flag}
\alias{flag}
\alias{flag,Status-method}
\alias{flag<-}
\alias{flag<-,Status-method}
\alias{obs}
\alias{obs,Status-method}
\alias{obs<-}
\alias{obs<-,Status-method}
\alias{app,Status-method}
\alias{timestamp,Status-method}
\title{Accessor functions for context objects.}
\description{

  These are basic accessor functions for the fields of an object of
  class \code{\linkS4class{Status}}.  Note that both the \code{flgs}
  and \code{observables} fields of the Status object are named lists.
  The functions \code{flag(x,name)} and \code{obs(x,name)} access a
  single component of those objects.
  
}
\usage{
flag(x, name)
\S4method{flag}{Status}(x, name)
flag(x, name) <- value
\S4method{flag}{Status}(x, name) <- value
obs(x, name)
\S4method{obs}{Status}(x, name)
obs(x, name) <- value
\S4method{obs}{Status}(x, name) <- value
\S4method{app}{Status}(x)
\S4method{timestamp}{Status}(x)
}
\arguments{
  \item{x}{A \code{\link{Status}} object whose fields will be accessed.}
  \item{name}{The name of the component for a \code{flag} or \code{obs}
    field.}
  \item{value}{The replacement value for the field.}
}
\value{

  The functions \code{flag} and \code{obs} return the named component
  of the \code{flags} or \code{observables} field of \code{x}
  respectively.  If no component will the given name exists, they return
  \code{NULL}.

  The functions \code{app}, \code{\link{context}}, \code{timestamp} and
  \code{\link{oldContext}} return the value of the corresponding field of
  \code{x}.

  The setter methods return the modified \code{\linkS4class{Status}}
  object.

}
\author{Russell Almond}
\seealso{

  \code{\linkS4class{Status}} describes the state object.  See
  \code{\link{context}}, \code{\link{context<-}}, and
  \code{\link{oldContext}} for other accessor methods shared by context
  and other objects.

   The functions \code{\link{setJS}}, \code{\link{getJS}} and
  \code{\link{removeJS}} provide mechanisms for accessing the fields of
  a status object from \code{\linkS4class{Rule}} \link{Conditions} and
  \code{Predicates}. 
 
  The following functions access the timer fields of the state object.
  \code{\link{timer}}, \code{\link{timerTime}},
  \code{\link{timerRunning}}, \code{\link{setTimer}}

}
\examples{

st <- Status("Phred","Level 0",timerNames="watch",
   flags=list("list"=list("one"=1,"two"="too"),"vector"=(1:3)*10),
   observables=list("numeric"=12.5,char="foo",
                 "list"=list("one"="a","two"=2),"vector"=(1:3)*100),
   timestamp=as.POSIXct("2018-12-21 00:01"))

stopifnot(
  uid(st) == "Phred",
  context(st)=="Level 0",
  oldContext(st)=="Level 0",
  all.equal(flag(st,"list"),list("one"=1,"two"="too")),
  all.equal(flag(st,"vector"),(1:3)*10),
  all.equal(obs(st,"numeric"),12.5),
  all.equal(obs(st,"char"),"foo"),
  all.equal(obs(st,"list"),list("one"="a","two"=2)),
  all.equal(obs(st,"vector"),(1:3)*100),
  all.equal(timestamp(st),as.POSIXct("2018-12-21 00:01"))
)

context(st) <- "Level 1"
stopifnot(
  context(st)=="Level 1",
  oldContext(st)=="Level 0")

stopifnot(is.null(flag(st,"numeric")))
flag(st,"numeric") <- 17L
stopifnot(!is.null(flag(st,"numeric")),
          flag(st,"numeric")==17L)

flag(st,"list")$two <- "two"
stopifnot(all.equal(flag(st,"list"),list("one"=1,"two"="two")))

obs(st,"vector")[2] <- 2
stopifnot(all.equal(obs(st,"vector"),c(100,2,300)))

}
\keyword{ manip }
\keyword{ interface }

