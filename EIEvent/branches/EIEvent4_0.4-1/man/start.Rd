\name{start}
\alias{start}
\alias{pause}
\alias{resume}
\alias{isRunning}
\alias{totalTime}
\alias{timeSoFar}
\alias{timeSoFar<-}
\alias{reset}
\alias{isRunning,Timer-method}
\alias{pause,Timer,POSIXt-method}
\alias{reset,Timer-method}
\alias{resume,Timer,POSIXt-method}
\alias{start,Timer,POSIXt-method}
\alias{timeSoFar,Timer,POSIXt-method}
\alias{timeSoFar<-,Timer,POSIXt-method}
\alias{totalTime,Timer-method}
\title{Functions for manipulating timer objects.}
\description{

  A \code{\link{Timer}} object keeps track of the elapsed time between
  events.  These functions update the state of the timer.  Note that
  because \code{Timer}s don't operate in real time, most of these
  functions need to be passed the \dQuote{current} time, which is the
  \code{\link[Proc4]{timestamp}} of the \code{\linkS4class{Event}} which
  is being processed.

}
\usage{
start(timer, time, runningCheck = TRUE)
\S4method{start}{Timer,POSIXt}(timer, time, runningCheck = TRUE)
pause(timer, time, runningCheck = TRUE)
\S4method{pause}{Timer,POSIXt}(timer, time, runningCheck = TRUE)
resume(timer, time)
\S4method{resume}{Timer,POSIXt}(timer, time)
isRunning(timer)
\S4method{isRunning}{Timer}(timer)
timeSoFar(timer, time)
\S4method{timeSoFar}{Timer,POSIXt}(timer, time)
\S4method{timeSoFar}{Timer,POSIXt}(timer, time) <- value
totalTime(timer)
\S4method{totalTime}{Timer}(timer)
reset(timer)
\S4method{reset}{Timer}(timer)
}
\arguments{
  \item{timer}{An object of class \code{\linkS4class{Timer}}.}
  \item{time}{Either an object of class \code{\link[base]{POSIXt}} or a
    an object that can be coerced into class \code{POSIXt}.  Note that
    strings need to be in ISO 8601 format (or manually converted using
    \code{\link[base]{strptime}}.)  This is usually the
    \code{\link[Proc4]{timestamp}} value of the
    \code{\linkS4class{Event}} being processed.}
  \item{runningCheck}{A logical value.  If \code{TRUE} \code{start},
    \code{pause} and \code{resume} will signal an error if the timer is
    currently in an unexpected state.}
  \item{value}{A time interval in \code{\link[base]{difftime}} format
    which is to be the new elapsed time.}
}
\details{

  These functions allow the \code{\linkS4class{Timer}} object to behave
  like a stopwatch, event though it is implemented with a collection of
  timestamps.  Because it doesn't really run a clock, but instead takes
  the different between the timestamps of the starting and finishing
  event, most of the function here need to pass the \dQuote{current}
  time, which is defined as the \code{\link[Proc4]{timestamp}} of the
  \code{\linkS4class{Event}} object being processed.

  The following functions are supported:
  \describe{
    \item{start}{Sets the timer running.  If \code{runningCheck} is
      true, it signals an error if the timer is already running.  Does
      not change the elapsed time.}
    \item{pause}{Pauses the timer. If \code{runningCheck} is
      true, it signals an error if the timer is not currently running.}
    \item{resume}{Identical to start with \code{runningCheck=TRUE}.}
    \item{timeSoFar}{Returns the elapsed time on the timer.  (Similar to
      the lap time on a stopwatch.)}
    \item{timeSoFar<-}{Sets the elapsed time to a new value.  If the
      timer is running, adjusts the start time to the current
      \code{time} argument.}
    \item{isRunning}{Returns the state of the timer as a logical value
      (\code{TRUE} for running).}
    \item{reset}{Stops the timer and sets the elapsed time to zero.}
    \item{totalTime}{Returns the total time up until the last pause;
      if the timer is running, the time since \code{startTime} is
      ignored.}
  }

}
\value{

  The \code{isRunning} function returns a logical value giving the state
  of the timer.

  The \code{timeSoFar} and \code{totalTime} functions returns an object
  of type \code{\link[base]{difftime}} giving the current elapsed time.

  The other functions return the modified \code{timer} object.

}
\author{Russell Almond}
\note{

  Internally, \code{\linkS4class{Timer}} objects maintain their state
  using two fields:  \code{startTime} and \code{totalTime}.  When the
  timer is started (resumed), the \code{startTime} is set to the current
  time, and it is set to \code{NA} when it is paused.  The running
  status can be determined by checking whether or not the
  \code{startTime} is NA.  When the timer is paused, the difference
  between the current time and the \code{startTime} is added to the
  \code{totalTime}. So the elapsed time is the \code{totalTime} plus the
  difference between the current \code{time} (the argument)  and the
  \code{startTime}. 

  Here are the acutal implementation of the manipulation functions:
  \describe{
    \item{start}{Sets the \code{startTime} to the \code{time}.  Does not
      change \code{totalTime}.}
    \item{pause}{Adds the difference between \code{time} and
      \code{startTime} to \code{totalTime}; sets \code{startTime} to
      \code{NA}.}
    \item{resume}{Identical to start with \code{runningCheck=TRUE}.}
    \item{timeSoFar}{If running, returns \code{totalTime} + \code{time}
      - \code{startTime}.  If paused, returns \code{totalTime}.}
    \item{timeSoFar<-}{Sets \code{totalTime} to \code{value}.  If the
      timer is running sets \code{startTime <- time}.}
    \item{isRunning}{Returns \code{!is.na(startTime)}.}
    \item{reset}{Sets \code{startTime} to \code{NA} and \code{totalTime}
      to 0 seconds.}
    \item{totalTime}{Returns the value of the \code{totalTime} field,
      ignoring the time elapsed between \code{time} and \code{startTime}.}
  }


}
\seealso{

  The \code{\linkS4class{Timer}} class describes timers, and the
  \code{\linkS4class{Status}} class contains a collection of timers.

  Methods for manipulating timers in states:
  \code{\link{timer}}, \code{\link{timerTime}}, and
  \code{\link{timerRunning}}

  The functions \code{\link{setJS}}, \code{\link{getJS}} and
  \code{\link{removeJS}} have details about how to manipulate timers
  using rules.


}
\examples{

## Create the timer
stopwatch <- Timer("stopwatch")
stopifnot(isRunning(stopwatch)==FALSE,
          timeSoFar(stopwatch,as.POSIXct("2018-12-21 00:01"))==
          as.difftime(0,units="mins"),
          totalTime(stopwatch)==as.difftime(0,units="mins"))

## Start the timer.
stopwatch <- start(stopwatch,as.POSIXct("2018-12-21 00:01"))
stopifnot(isRunning(stopwatch)==TRUE,
          timeSoFar(stopwatch,as.POSIXct("2018-12-21 00:01"))==
          as.difftime(0,units="mins"),
          totalTime(stopwatch)==as.difftime(0,units="mins"))


## Note that time so far is based on the time argument.
stopifnot(isRunning(stopwatch)==TRUE,
          timeSoFar(stopwatch,as.POSIXct("2018-12-21 00:10"))==
          as.difftime(9,units="mins"),
          totalTime(stopwatch)==as.difftime(0,units="mins"))

## Pause the timer.
stopwatch <- pause(stopwatch,as.POSIXct("2018-12-21 00:10"))
stopifnot(isRunning(stopwatch)==FALSE,
          timeSoFar(stopwatch,as.POSIXct("2018-12-21 00:10"))==
          as.difftime(9,units="mins"),
          totalTime(stopwatch)==as.difftime(9,units="mins"))

## adjust the time.
timeSoFar(stopwatch,as.POSIXct("2018-12-21 00:10")) <-
    as.difftime(10,units="mins")
stopifnot(isRunning(stopwatch)==FALSE,
          timeSoFar(stopwatch,as.POSIXct("2018-12-21 00:10"))==
          as.difftime(10,units="mins"),
          totalTime(stopwatch)==as.difftime(10,units="mins"))

## resume the timer and adjust the time again.
stopwatch <- resume(stopwatch,as.POSIXct("2018-12-21 01:01"))
timeSoFar(stopwatch,as.POSIXct("2018-12-21 01:10")) <-
    as.difftime(5,units="mins")
stopifnot(isRunning(stopwatch)==TRUE,
          timeSoFar(stopwatch,as.POSIXct("2018-12-21 01:11"))==
          as.difftime(6,units="mins"),
          totalTime(stopwatch)==as.difftime(5,units="mins"))

## Reset the timer
stopwatch <- reset(stopwatch)
stopifnot(isRunning(stopwatch)==FALSE,
          timeSoFar(stopwatch,as.POSIXct("2018-12-21 00:01"))==
          as.difftime(0,units="mins"),
          totalTime(stopwatch)==as.difftime(0,units="mins"))


}
\keyword{ mainp }
\keyword{ chron }
