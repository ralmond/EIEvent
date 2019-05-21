\name{setTimer}
\alias{setTimer}
\alias{setTimer,Status,character-method}
\alias{timer}
\alias{timer<-}
\alias{timerTime}
\alias{timerTime,Status,character-method}
\alias{timerTime<-}
\alias{timerTime<-,Status,character-method}
\alias{timerRunning}
\alias{timerRunning,Status,character-method}
\alias{timerRunning<-}
\alias{timerRunning<-,Status,character-method}
\title{Manipulates a Timer inside of a Status}
\description{

  A \code{\linkS4class{Status}} object contains a named collection of
  \code{\linkS4class{Timer}} objects.  These functions access the timer
  object.  Note that because the timer is not an actual clock, but
  rather calculates the time difference between events, most of the
  functions must be passed the \dQuote{current} time, which is usually
  the \code{\link[Proc4]{timestamp}} of the \code{\linkS4class{Event}}
  object being processed.

}
\usage{
setTimer(x, timerID, time, running, now)
\S4method{setTimer}{Status,character}(x, timerID, time, running, now)
timer(x, name)
timer(x, name) <- value
timerTime(x, name, now)
\S4method{timerTime}{Status,character}(x, name, now)
timerTime(x, name, now) <- value
\S4method{timerTime}{Status,character}(x, name, now) <- value
\S4method{timerRunning}{Status,character}(x, name, now)
timerRunning(x, name, now) <- value
\S4method{timerRunning}{Status,character}(x, name, now) <- value

}
\arguments{
  \item{x}{An object of class \code{\linkS4class{Status}} whose timers
    are to be accessed.  (These are generic functions, so methods for
    classes other than \code{Status} could be written.)}
  \item{timerID}{A character string of the form
    \code{state.timers.}\emph{name}.  The name operates like the name
    argument.} 
  \item{name}{A character scalar giving the name of the timer to be
    accessed. }
  \item{time}{The new elapsed time of the timer.  This 
    should be an object of class \code{\link[base]{difftime}}.}
  \item{running}{A logical flag indicating whether or not the timer
    should be running.}
  \item{now}{The current time, usually from the
    \code{\link[Proc4]{timestamp}} of the \code{\linkS4class{Event}} 
    object being processed.}
  \item{value}{The replacement value.  For \code{timerTime<-} this
    should be an object of class 
    \code{\link[base]{difftime}}.  For \code{timerRunning<-} this should
    be a logical value.  For \code{timer<-} this should be an object of
    class \code{\linkS4class{Timer}}.}
}
\details{

  The \code{\linkS4class{Status}} objects contain a named list of
  \code{\linkS4class{Timer}} objects.  Each timer contains two
  conceptual fields:  \code{running} which indicates whether or not the
  timer is running and \code{time} which indicates the current elapsed
  time.  (Note that these are actually implemented using differences
  between timestamps, which is why most of the functions need to pass
  the current time in the \code{now} argument.  See \code{\link{start}}
  for details.

  The \code{timer} and \code{timer<-} functions access the
  \code{\linkS4class{Timer}} object directly.

  The \code{timerRunning} and \code{timerRunning<-} functions access 
  the conceptual \code{running} field of the timer.  In particular, the
  setter method \code{\link{start}}s or \code{\link{pause}}s the timer.

  The \code{timerTime} and \code{timerTime<-} functions access 
  the conceptual \code{time} field of the timer.  In particular, they
  call \code{\link{timerTime}} or \code{\link{timerTime}} on the timer. 

  The function \code{setTimer} is an omnibus modifier, meant to be
  called from \code{\link{setJS}}.  Instead of the timer name, it uses
  the fully qualified dot notation:  \code{state.timers.}\emph{name}.
  If no timer for the given name exists, it creates one; otherwise, it
  uses the existing timer.  It then calls \code{timerTime<-} and
  \code{timerRunning<-} with the given value.

  The functions \code{\link{getJS}} and \code{\link{setJS}} call these
  functions if the referenced field is of the form
  \code{state.timers.}\emph{name}.  Note that the \code{setJS} function
  gets the current time from the event object, so it does not need to be
  specified in this form.

  
}
\value{

  The function \code{timer} returns an object of class
  \code{\linkS4class{Timer}}.

  The function \code{timerRunning} returns a logical value indicating
  whether or not the timer is currently running.

  The function \code{timerTime} returns the elapsed time in
  \code{\link[base]{difftime}} format.

  The function \code{setTimer} and the other setter methods return the
  \code{\linkS4class{Status}} object which is the first argument.

}
\author{Russell Almond}
\note{

  Internally, the timers are implemented as a start time and an elapsed
  time (see \code{\link{start}}).  Elapsed times are calculated by
  differencing two time stamps.  Therefore, it is usually necessary to
  pass along the \dQuote{current} time with these functions, usually
  from the timestamp of the \code{\linkS4class{Event}} object.

}
\seealso{

  The \code{\linkS4class{Timer}} class describes timers, and the
  \code{\linkS4class{Status}} class contains a collection of timers.

  Methods for manipulating timer directly include
  \code{\link{start}}, \code{\link{resume}}, \code{\link{pause}}, 
  \code{\link{isRunning}}, \code{\link{timeSoFar}} and
  \code{\link{reset}}. 

  The functions \code{\link{setJS}}, \code{\link{getJS}} and
  \code{\link{removeJS}} have details about how to manipulate timers
  using rules.


}
\examples{

st <- Status("Phred","Level 0",timerNames="watch",
   timestamp=as.POSIXct("2018-12-21 00:00:01"))
context(st) <- "Level 1"

stopifnot(timer(st,"watch")@name=="watch")

timer(st,"stopwatch") <- Timer("stopwatch")
stopifnot(timer(st,"stopwatch")@name=="stopwatch")

timerRunning(st,"stopwatch",as.POSIXct("2018-12-21 00:00:01")) <- TRUE
stopifnot(!timerRunning(st,"watch",as.POSIXct("2018-12-21 00:00:02")),
           timerRunning(st,"stopwatch",as.POSIXct("2018-12-21 00:00:02")))

timerRunning(st,"stopwatch",as.POSIXct("2018-12-21 00:01:01")) <- FALSE
stopifnot(!timerRunning(st,"stopwatch",as.POSIXct("2018-12-21 00:01:02")),
           timerTime(st,"stopwatch",as.POSIXct("2018-12-21 00:01:02"))==
           as.difftime(1,units="mins"))

timerRunning(st,"stopwatch",as.POSIXct("2018-12-21 00:03:01")) <- TRUE
stopifnot(timerTime(st,"watch",as.POSIXct("2018-12-21 00:03:02"))==
          as.difftime(0,units="secs"),
          timerTime(st,"stopwatch",as.POSIXct("2018-12-21 00:03:02"))==
          as.difftime(61,units="secs"))


timerTime(st,"watch",as.POSIXct("2018-12-21 00:05:00")) <-
         as.difftime(5,units="mins")
timerTime(st,"stopwatch",as.POSIXct("2018-12-21 00:05:00")) <-
         as.difftime(5,units="mins")
stopifnot(timerTime(st,"watch",as.POSIXct("2018-12-21 00:05:02"))==
          as.difftime(300,units="secs"),                  
          timerTime(st,"stopwatch",as.POSIXct("2018-12-21 00:05:02"))==
          as.difftime(302,units="secs"))


st <- setTimer(st,"state.timers.stopwatch",as.difftime(0,units="secs"),FALSE,
               as.POSIXct("2018-12-21 00:10:00"))
st <- setTimer(st,"state.timers.runwatch",as.difftime(50,units="secs"),TRUE,
               as.POSIXct("2018-12-21 00:10:00"))
stopifnot(!timerRunning(st,"stopwatch",as.POSIXct("2018-12-21 00:10:02")),
           timerRunning(st,"runwatch",as.POSIXct("2018-12-21 00:10:02")),
           timerTime(st,"stopwatch",as.POSIXct("2018-12-21 00:10:02"))==
           as.difftime(0,units="secs"),
           timerTime(st,"runwatch",as.POSIXct("2018-12-21 00:10:02"))==
           as.difftime(52,units="secs"))

}
\keyword{ manip }
\keyword{ chron }
