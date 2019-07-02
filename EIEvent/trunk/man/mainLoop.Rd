\name{mainLoop}
\alias{mainLoop}
\title{Runs the \code{EIEngine} as a server.}
\description{

  The \code{mainLoop} is used when the \code{\linkS4class{EIEngine}} is
  used as a server.  It checks the database, for unprocessed
  \code{\linkS4class{Event}} objects, and calls
  \code{\link{handleEvent}} on them in the order of their
  \code{\link[Proc4]{timestamp}}s.  As a server, this is potentially an
  infinite loop, see details for ways of gracefully terminating the loop.

}
\usage{
mainLoop(eng)
}
\arguments{
  \item{eng}{An instance of \code{\linkS4class{EIEngine}} which will be
    used as a server.}
}
\details{

  The \code{\linkS4class{EIEngine}} class uses the Events collection in
  the database (\code{eng$eventdb()}) as a queue.  All events have a
  \code{\link[Proc4]{processed}} field which is set to true when the
  event is processed.  Thus the main loop iterates over the following
  three statements:
  \enumerate{
    \item{Fetch the oldest unprocessed Event: \code{eve <-
        eng$fetchNextEvent()}.}
    \item{Process the event: \code{out <- handleEvent(eng,eve)}.  (Note:
      this expression will always return.  If it generates an error, the 
      error will be logged and an object of class \code{try-error} will be
      returned.)} 
    \item{Mark the event as processed: \code{eng$setProcessed(eve)}.}
  }

  At its simplest level, the funciton produces an infinite loop over
  these three statements, with some additional steps related to
  logging and control.

  First, if the event queue is empty, the process sleeps for a time
  given by \code{eng$waittime} and then checks the queue again.  At the
  same time, it checks status of the active flag for the process using
  the \code{eng$isActivated()} call.  By default this checks the
  \code{active} field of the record corresponding to \code{app(eng)} in
  the collection \code{AuthorizedApps} in the database \code{Proc4}.
  Setting that field to false manually will result in the
  \code{mainLoop} terminating when the queue is empty.  As R is running
  in server mode when this happens, this often needs to be done using an
  external process.  The following command issues from the Mongo shell
  will shut down the server for an application containing the string
  "appName" as part of its name.

  \code{db.AuthorizedApps.update(\{app:\{$regex:"appName"\}\},
    \{$set:\{active:false\}\});}

  To facilitate testing, the field \code{eng$processN} can be set to a
  finite value.  This number is decremented at every cycle, and when
  it reaches 0, the \code{mainLoop} is terminated, whether or not
  their are any remaining events to be processed.  Setting
  \code{eng$processN} to an infinite value, will result in an infinite
  loop that can only be stopped by using the active flag (or
  interrupting the process).

}
\value{

  There is no return value.  The function is used entirely for its side
  effects. 

}
\references{

  The document \dQuote{Rules Of Evidence} gives extensive documentation
  for the rule system.
  \url{https://pluto.coe.fsu.edu/Proc4/RulesOfEvidence.pdf}. 

  There is a \dQuote{quick start} document which describes how to set up the
  engine.  This is available at 
  \url{https://pluto.coe.fsu.edu/Proc4/EIQuickStart.pdf}. 

}
\author{Russell Almond}
\note{

  Currently, when running in server model (i.e., with
  \code{eng$processN} set to infinity), there are two ways of stopping
  the engine:  a clean stop after all events are processed using the
  \code{active} flag, and an immediate stop, possibly mid cycle, by
  killing the server process.  It became apparent during testing that
  there was a need for a graceful but immediate stop, i.e., a stop after
  processing the current event.  This should appear in later versions.

}
\seealso{

  The class \code{\linkS4class{EIEngine}} describes the setup of the
  engine, and the function \code{\link{handleEvent}} describes the
  processing that occurs for each event.
  
}
\examples{

\dontrun{
## From EIEvent.R script
app <- "ecd://epls.coe.fsu.edu/P4test"
loglevel <- "DEBUG"
cleanFirst <- TRUE
eventFile <- "/home/ralmond/Projects/EvidenceID/c081c3.core.json"

## Initialization details (from EIini.R script)
EIeng.common <- list(host="localhost",username="EI",password="secret",
                     dbname="EIRecords",P4dbname="Proc4",waittime=.25)
appstem <- basename(app)
EIeng.params <- list(app=app)
logfile <- file.path("/usr/local/share/Proc4/logs",
                     paste("EI_",appstem,"0.log",sep=""))
EI.listenerSpecs <-
  list("InjectionListener"=list(sender=paste("EI",appstem,sep="_"),
            dbname="EARecords",dburi="mongodb://localhost",
            colname="EvidenceSets",messSet="New Observables"))

## Setup logging
if (interactive()) {
  flog.appender(appender.tee(logfile))
} else {
  flog.appender(appender.file(logfile))
}
flog.threshold(loglevel)
## Setup Listeners
listeners <- lapply(names(EI.listenerSpecs),
                    function (ll) do.call(ll,EI.listenerSpecs[[ll]]))
names(listeners) <- names(EI.listenerSpecs)
if (interactive()) {
  cl <- new("CaptureListener")
  listeners <- c(listeners,cl=cl)
}
## Make the EIEngine
eng <- do.call(EIEngine,c(EIeng.params,list(listeners=listeners),
                          EIeng.common))
## Clean out old records from the database.
if (cleanFirst) {
  eng$eventdb()$remove(buildJQuery(app=app(eng)))
  eng$userRecords$clearAll(FALSE)   #Don't clear default
  eng$listenerSet$messdb()$remove(buildJQuery(app=app(eng)))
  for (lis in eng$listenerSet$listeners) {
    if (is(lis,"UpdateListener") || is(lis,"InjectionListener"))
      lis$messdb()$remove(buildJQuery(app=app(eng)))
  }
}
## Process Event file if supplied
if (!is.null(eventFile)) {
  system2("mongoimport",
          sprintf('-d %s -c Events --jsonArray', eng$dbname),
          stdin=eventFile)
  ## Count the number of unprocessed events
  NN <- eng$eventdb()$count(buildJQuery(app=app(eng),processed=FALSE))
}
if (!is.null(eventFile)) {
  ## This can be set to a different number to process only a subset of events.
  eng$processN <- NN
}
## Activate engine (if not already activated.)
eng$activate()
mainLoop(eng)

## Depending on value of NN, this may not terminate!
}
}
\keyword{ database }
\keyword{ manip }
