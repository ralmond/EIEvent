\name{Timer-class}
\Rdversion{1.1}
\docType{class}
\alias{Timer-class}

\title{Class \code{"Timer"}}
\description{

  A \code{Timer} measures the time between events in an event sequence.
  Rather than containing an actual timer, it works by subtracting start
  and stop times from different events.  Therefore \dQuote{starting} a
  timer sets the start time to the current time (as measured by the most
  recent event), and reading the elapsed time at an event, looks at the
  difference between the start time and the stop time (as measured by
  the current event).

}
\section{Objects from the Class}{
Objects can be created by calls of to the\code{\link{Timer}(...)}
function.  They are also created by the \code{\link{Status}} constructor
when called with names for the timer objects.

}
\section{Slots}{
  \describe{
    \item{\code{name}:}{Object of class \code{"character"} giving an
      identifier for the timer.  Used in error reporting}
    \item{\code{startTime}:}{Object of class \code{"POSIXct"}:  the time
    at which the timer was started.  If the timer is not running, this
    will be \code{NA}.}
    \item{\code{totalTime}:}{Object of class \code{"difftime"}: the
      total elapsed time prior to the last start/resume call. }
  }
}
\section{Methods}{
  \describe{
    \item{as.jlist}{\code{signature(obj = "Timer", ml = "list")}:
      converts the Timer object into a form to be serialized as a JSON
      object.  See \code{\link[Proc4]{as.jlist}}. }
    \item{isRunning}{\code{signature(timer = "Timer")}: returns
      \code{TRUE} if the timer is running and \code{FALSE} if not. }
    \item{pause}{\code{signature(timer = "Timer", time = "POSIXct")}:
      Pauses the timer and sets the accumulated time to the elapsed time
      so far.}
    \item{reset}{\code{signature(timer = "Timer")}: Pauses the timer and
      sets the accumulated time to zero.}
    \item{resume}{\code{signature(timer = "Timer")}: Puts the timer back
      in the running state and does not affect the total elapsed time.}
    \item{start}{\code{signature(timer = "Timer")}: Starts the timer. }
    \item{timeSoFar}{\code{signature(timer = "Timer")}: Returns the
      elapsed time. }
    \item{timeSoFar<-}{\code{signature(timer = "Timer")}: Sets the
      elapsed time. }
    \item{totalTime}{\code{signature(timer = "Timer")}: Returns the
      current value of the total time field. }
  }
}
\section{Details}{

  The timer is not actually running a clock.  It is instead counting the
  elapsed time between events.  This primarily works by setting the
  \code{startTime} field when the timer is \dQuote{started} and then
  differencing this from the current time as measured by the timestamp
  of the currently processed \code{\linkS4class{Event}}.

  This should be transparent for most uses, with one note.  Methods like
  \code{\link{start}}, \code{\link{pause}}, \code{\link{resume}}, and
  \code{\link{timeSoFar}} need to be passed the current time, so the
  \code{Timer} can adjust its internal state.

  In the \code{\link{getJS}} and \code{\link{setJS}} functions, the
  timer behaves as if it has two virtual fields:  \code{.run} or
  \code{.running} and \code{.time} or \code{.value}.  The \code{.run}
  field returns the value of \code{\link{isRunning}}, and setting it
  to \code{FALSE} will cause the timer to \code{\link{pause}} and
  setting it to \code{TRUE} will cause the timer to \code{\link{start}}
  or \code{\link{resume}}.  The \code{.time} or \code{.value} field
  returns the \code{\link{timeSoFar}} field of the timer.  Setting it
  adjusts the \code{totalTime} without affecting the running state.  
}
\author{Russell Almond}
\note{

  For those who need a more detailed understanding, the timer works with
  two fields:  \code{startTime} and \code{totalTime}.  Intially
  \code{startTime} starts as \code{NA} and \code{totalTime} is 0.  On a
  call to \code{start} or \code{resume}, the \code{startTime} is set to
  the current time.  On a call to \code{pause} the difference between
  the \code{startTime} and the current time (passed as an argument) is
  added to the \code{totalTime}, and \code{startTime} is set to
  \code{NA}.

  This means that \code{isRunning} is essentially
  \code{!is.na(startTime)}.  If the timer is paused, the
  \code{timeSoFar} is the \code{totalTime}.  If the timer is running,
  the \code{timeSoFar} is the \code{totalTime} plus the difference
  between the current time (passed as an argument) and the
  \code{startTime}. The setter \dQuote{\code{timeSoFar<-}} will adjust
  \code{startTime} to the current \code{time} argument if the timer is
  running. 

  The \code{\link{setJS}} function gets the current time from the
  current \code{\linkS4class{Event}} object being processed, and thus
  automatically takes care of the time argument.

}
\seealso{

  The funciton \code{\link{Timer}} is the constructor, and the function
  \code{\link{parseTimer}} builds a \code{Timer} from JSON data.

  The \code{\linkS4class{Status}} class contains a collection of
    timers.  Other classes in the EIEvent system:
  \code{\linkS4class{EIEngine}}, \code{\linkS4class{Context}},
  \code{\linkS4class{Rule}}, \code{\linkS4class{Event}},
  \code{\linkS4class{RuleTable}}.

  The functions \code{\link{setJS}}, \code{\link{getJS}} and
  \code{\link{removeJS}} have details about how to manipulate timers
  using rules.

  Methods for working with Timers:
  \code{\link{start}}, \code{\link{pause}}, \code{\link{resume}},
  \code{\link{isRunning}}, \code{\link{totalTime}},
  \code{\link{timeSoFar}}, and \code{\link{reset}}

  Methods for manipulating timers in states:
  \code{\link{timer}}, \code{\link{timerTime}}, and
  \code{\link{timerRunning}}


}
\examples{
showClass("Timer")
}
\keyword{classes}
\keyword{ chron }
