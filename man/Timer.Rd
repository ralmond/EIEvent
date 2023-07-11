\name{Timer}
\alias{Timer}
\alias{parseTimer}
\alias{as.jlist,Timer,list-method}
\title{Constructor for Timer objects.}
\description{

  The \code{Timer} funciton is the constructor for the
  \code{\linkS4class{Timer}} object.  Timer objects are usually part of
  a \code{\linkS4class{Status}} object.  The \code{parseTimer} function
  recreates a timer from a JSON list and is used as part of
  \code{\link{parseStatus}}.

}
\usage{
Timer(name)
parseTimer(rec)
\S4method{as.jlist}{Timer,list}(obj, ml, serialize = TRUE)
}
\arguments{
  \item{name}{A character scalar giving the name of the timer.  This is
    used for documentation only, but it is most useful if it matches the
    name it is given in the \code{\linkS4class{Status}} object.}
  \item{rec}{A named list containing JSON data.}
  \item{obj}{An object of class \code{\linkS4class{Event}} to be
    encoded.}
  \item{ml}{A list of fields of \code{obj}.  Usually, this is created by
    using \code{\link[base]{attributes}(obj)}.}
  \item{serialize}{A logical flag. If true,
    \code{\link[jsonlite]{serializeJSON}} is used to protect the
    \code{data} field (and other objects which might contain complex R
    code.}
}
\details{

  The \code{Timer} function creates a new timer with zero elapsed time
  and in the paused (not running) state.  Normally, this is not called
  directly, but through either the \code{\link{Status}} constructor or
  the \code{\link{setTimer}} function.

  The function \code{as.jlist} converts the \code{obj} into a named
  list. It is usually called from the \code{as.jlist} function applied
  to a \code{\linkS4class{Status}} object, which is in turn usually
  called from   \code{\link[mongo]{as.json}}.

  The \code{parseTimer} function is the inverse of \code{as.jlist}
  applied to atimer object.  It is designed to be called by the
  function, \code{\link{parseStatus}}, which is given as an argument to
  \code{\link[mongo]{getOneRec}}, \code{\link[mongo]{getManyRecs}}


}
\value{

  The functions \code{Timer} and \code{parseTimer} return objects of
  class timer.  The function \code{as.jlist} produces a named list
  suitable for passing to \code{\link[jsonlite]{toJSON}}.

}
\author{Russell Almond}
\note{

  See \code{\link{start}} for information about how the timer is
  actually implemented.

}
\seealso{
  \code{\linkS4class{Timer}} describes the timer object, and
  \code{\linkS4class{Status}} describes the status object which contains
  it.

  Methods for mainpulating timers:
  \code{\link{start}}, \code{\link{pause}}, \code{\link{resume}},
  \code{\link{isRunning}}, \code{\link{totalTime}},
  \code{\link{timeSoFar}}, and \code{\link{reset}}

  Methods for manipulating timers in states:
  \code{\link{timer}}, \code{\link{timerTime}}, and
  \code{\link{timerRunning}}

  \code{\link[Proc4]{buildMessage}} and \code{\link[mongo]{as.json}}
  describe the JSON conversion system.

  The functions \code{\link[mongo]{getOneRec}} and
  \code{\link[mongo]{getManyRecs}} use \code{parseEvent} to extract
  events from a database.


}
\examples{
sw <- Timer("stopwatch")
swa <- parseTimer(as.jlist(sw,attributes(sw)))
stopifnot(isRunning(swa)==FALSE,
          timeSoFar(swa,as.POSIXct("2018-12-21 00:01"))==
          as.difftime(0,units="mins"),
          totalTime(swa)==as.difftime(0,units="mins"))

## Start the timer.
sw <- start(sw,as.POSIXct("2018-12-21 00:01"))
swa <- parseTimer(as.jlist(sw,attributes(sw)))
## Note that time so far is based on the time argument.
stopifnot(isRunning(swa)==TRUE,
          timeSoFar(swa,as.POSIXct("2018-12-21 00:10"))==
          as.difftime(9,units="mins"),
          totalTime(swa)==as.difftime(0,units="mins"))

## Pause the timer.
sw <- pause(sw,as.POSIXct("2018-12-21 00:10"))
swa <- parseTimer(as.jlist(sw,attributes(sw)))
stopifnot(isRunning(swa)==FALSE,
          timeSoFar(swa,as.POSIXct("2018-12-21 00:10"))==
          as.difftime(9,units="mins"),
          totalTime(sw)==as.difftime(9,units="mins"))


}
\keyword{ interface }
\keyword{ chron }

